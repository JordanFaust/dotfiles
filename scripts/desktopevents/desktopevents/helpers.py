import json
import datetime
import urllib

from adal import AuthenticationContext
import pyperclip
import requests
from dateutil import tz
import calendar

API_VERSION = 'v1.0'
RESOURCE = 'https://graph.microsoft.com'

def api_endpoint(url):
    """Convert a relative path such as /me/photo/$value to a full URI based
    on the current RESOURCE and API_VERSION settings in config.py.
    """
    if urllib.parse.urlparse(url).scheme in ['http', 'https']:
        return url # url is already complete
    return urllib.parse.urljoin(f'{RESOURCE}/{API_VERSION}/',
                                url.lstrip('/'))
def get_primary_inbox(token):
    session = requests.Session()
    session.headers.update({
        'Authorization': f'Bearer {token["accessToken"]}',
        'SdkVersion': 'sample-python-adal',
        'x-client-SKU': 'sample-python-adal',
    })

    response = session.get(api_endpoint("me/mailFolders"),
                        headers={'Content-Type': 'application/json'}
    )

    primaryInbox = {}
    for inbox in response.json()['value']:
        if inbox['displayName'] == 'Inbox':
            primaryInbox = inbox

    return primaryInbox

def get_unread_emails(inbox, token, log):
    session = requests.Session()
    session.headers.update({
        'Authorization': f'Bearer {token["accessToken"]}',
        'SdkVersion': 'sample-python-adal',
        'x-client-SKU': 'sample-python-adal',
        'Prefer': 'outlook.body-content-type=text'
    })

    time = datetime.datetime.now() - datetime.timedelta(days = 1)
    filter = f'isRead eq false and receivedDateTime ge {time.strftime("%Y-%m-%d")}'
    select = "receivedDateTime,subject"
    endpoint = f'me/mailFolders/{inbox["id"]}/messages?$filter={filter}&$select={select}'
    response = session.get(api_endpoint(endpoint),
                        headers={'Content-Type': 'application/json'}
    )

    if 'value' in response.json():
        return response.json()['value']
    else:
        return {}


def get_email_count(inbox, token):
    session = requests.Session()
    session.headers.update({
        'Authorization': f'Bearer {token["accessToken"]}',
        'SdkVersion': 'sample-python-adal',
        'x-client-SKU': 'sample-python-adal',
        'Prefer': 'outlook.body-content-type=text'
    })

    time = datetime.datetime.now() - datetime.timedelta(days = 1)
    filter = f'isRead eq false and receivedDateTime ge {time.strftime("%Y-%m-%d")}'
    select = "receivedDateTime,subject"
    endpoint = f'me/mailFolders/{inbox["id"]}/messages?$filter={filter}&$select={select}'
    response = session.get(api_endpoint(endpoint),
                        headers={'Content-Type': 'application/json'}
    )

    if 'value' in response.json():
        return len(response.json()['value'])
    else:
        return 0

def get_meetings(token, log):
    session = requests.Session()
    session.headers.update({
        'Authorization': f'Bearer {token["accessToken"]}',
        'SdkVersion': 'sample-python-adal',
        'x-client-SKU': 'sample-python-adal',
        'Prefer': 'outlook.timezone="Central Standard Time"'
    })

    today = datetime.datetime.utcnow().date()
    start_of_day = datetime.datetime(today.year, today.month, today.day, tzinfo=tz.tzutc()).astimezone(tz.gettz('America/Chicago'))
    start = datetime.datetime.today()
    end = start_of_day + datetime.timedelta(1)
    recurring_event_start = datetime.datetime.today() - datetime.timedelta(days=365)
    datetime_filter = f'(start/dateTime ge \'{start_of_day.strftime("%Y-%m-%dT%H:%M:%S")}\' and end/dateTime le \'{end.strftime("%Y-%m-%dT%H:%M:%S")}\')'
    recurring_event_startdate_filter = f'start/dateTime ge \'{recurring_event_start.strftime("%Y-%m-%dT%H:%M:%S")}\''
    recurring_event_enddate_filter = f'end/dateTime le \'{end.strftime("%Y-%m-%dT%H:%M:%S")}\''
    recurring_event_filter = f'(type eq \'seriesMaster\' and ({recurring_event_startdate_filter} and {recurring_event_enddate_filter}))'
    filter = f'{datetime_filter} or {recurring_event_filter}'
    select = "subject,start,end,location,type,recurrence"
    endpoint = f'me/events?$top=100&$select={select}&$filter={filter}&$orderBy=start/dateTime'
    response = session.get(api_endpoint(endpoint),
                        headers={'Content-Type': 'application/json'}
    )

    events = []
    for event in response.json()['value']:
        recurrence = event['recurrence']
        skipped = False
        if recurrence != None and recurrence["range"]["type"] != None:
            # Filter out meetings that are recurring that have already ended
            if recurrence['range']['type'] not in ['noEnd', 'numbered']:
                recurrence_end_date = datetime.datetime.strptime(recurrence["range"]["endDate"], '%Y-%m-%d').astimezone(tz.gettz('America/Chicago'))

                if recurrence_end_date < start_of_day:
                    log.debug(f'skipping meeting due to recurrence end date: {event["subject"]}')
                    skipped = True
                    continue

            # Filter out meetings that are recurring and numbered
            if recurrence['range']['type'] in ['numbered']:
                numbered_start_date = datetime.datetime.strptime(recurrence["range"]["startDate"], '%Y-%m-%d').astimezone(tz.gettz('America/Chicago'))
                interval_in_days = 1
                if recurrence['pattern']['type'] == 'weekly':
                    interval_in_days = 7

                numbered_end_date = numbered_start_date + datetime.timedelta(days=interval_in_days)

                if numbered_end_date < start_of_day:
                    log.info(f'skipping meeting due to numbered end date: {event["subject"]}')
                    skipped = True
                    continue

            # Filter out recurring meetings that are not scheduled for the current day
            recurrence_days = recurrence['pattern']['daysOfWeek']
            today_name = calendar.day_name[start.weekday()].lower()
            if today_name not in recurrence_days:
                log.debug(f'skipping meeting due to recurrence day of week: {event["subject"]}')
                skipped = True
                continue


            # Filter out recurring events that happen at a week interval of more than one and don't
            # fall on today. Calculating this is done by taking the modulo of the start week number
            # and the interval. If it is equal to 0 then the interval falls on this week.
            if recurrence['pattern']['type'] == 'weekly':
                interval = recurrence['pattern']['interval']
                interval_start_date = datetime.datetime.strptime(recurrence['range']['startDate'], '%Y-%m-%d')
                interval_start_week_number = interval_start_date.isocalendar()[1]
                current_week_number = start_of_day.isocalendar()[1]

                if interval_start_week_number % interval != 0:
                    log.debug(f'skipping meeting due to interval week number: {event["subject"]}')
                    skipped = True
                    continue

            if skipped != True:
                events.append(event)
        else:
            events.append(event)

    return sorted(events, key=lambda event: meeting_time_sort(event))

def meeting_time_sort(event):
    start = datetime.datetime.strptime(event['start']['dateTime'], '%Y-%m-%dT%H:%M:%S.%f0')
    return int(start.hour) * 60 + int(start.minute)
