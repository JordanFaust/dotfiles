import pprint

# import config
from adal import AuthenticationContext

from .helpers import get_unread_emails, get_email_count, get_meetings, get_primary_inbox

RESOURCE = 'https://graph.microsoft.com'
AUTHORITY_URL = 'https://login.microsoftonline.com/common'

def unread_emails(application_id, email, password, log):
    context = AuthenticationContext(AUTHORITY_URL, api_version=None)

    token = context.acquire_token_with_username_password(
        RESOURCE,
        email,
        password,
        application_id
    )

    inbox = get_primary_inbox(token)
    return get_unread_emails(inbox, token, log)


def email_count(application_id, email, password):
    context = AuthenticationContext(AUTHORITY_URL, api_version=None)

    token = context.acquire_token_with_username_password(
        RESOURCE,
        email,
        password,
        application_id
    )

    inbox = get_primary_inbox(token)
    return get_email_count(inbox, token)

def meetings(application_id, email, password, log):
    context = AuthenticationContext(AUTHORITY_URL, api_version=None)

    token = context.acquire_token_with_username_password(
        RESOURCE,
        email,
        password,
        application_id
    )

    return get_meetings(token, log)


if __name__ == '__main__':
    context = AuthenticationContext(AUTHORITY_URL, api_version=None)
