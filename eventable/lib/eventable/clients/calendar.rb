require 'googleauth'
require 'googleauth/stores/file_token_store'
require 'google/apis/calendar_v3'
require 'fileutils'
require 'os'

module Eventable
  module Clients
    class Calendar

      OOB_URI = 'urn:ietf:wg:oauth:2.0:oob'
      CalendarV3 = Google::Apis::CalendarV3

      def initialize()
      end

      def list(params)
        calendar = CalendarV3::CalendarService.new
        calendar.authorization = user_credentials_for(CalendarV3::AUTH_CALENDAR)

        page_token = nil
        limit = params[:limit] || 1000
        time_min = DateTime.iso8601(params[:start_time] || "2019-08-07T00:00:00Z").to_s
        time_max = DateTime.iso8601(params[:end_time] || "2019-08-10T00:00:00Z").to_s

        events = []
        begin
          result = calendar.list_events(
            'primary',
            max_results: [limit, 100].min,
            single_events: true,
            order_by: 'startTime',
            time_min: time_min,
            time_max: time_max,
            page_token: page_token,
            fields: 'items(id,summary,start,end,location),next_page_token'
          )

          result.items.each do |event|
            event = {
              "summary" => event.summary.to_s,
              "start_time" => event.start.date_time.to_s || event.start.date.to_s,
              "end_time" => event.end.date_time.to_s || event.end.date.to_s,
              "location" => event.location.to_s
            }
            events << event
          end

          limit -= result.items.length
          if result.next_page_token
            page_token = result.next_page_token
          else
            page_token = nil
          end
        end while !page_token.nil? && limit > 0

        return events
      end

      private

      # Returns the path to the client_secrets.json file.
      def client_secrets_path
        return ENV['GOOGLE_CLIENT_SECRETS'] if ENV.has_key?('GOOGLE_CLIENT_SECRETS')
        return well_known_path_for('client_secrets.json')
      end

      # Returns the path to the token store.
      def token_store_path
        return ENV['GOOGLE_CREDENTIAL_STORE'] if ENV.has_key?('GOOGLE_CREDENTIAL_STORE')
        return well_known_path_for('credentials.yaml')
      end

      # Builds a path to a file in $HOME/.config/google (or %APPDATA%/google,
      # on Windows)
      def well_known_path_for(file)
        if OS.windows?
          dir = ENV.fetch('HOME'){ ENV['APPDATA']}
          File.join(dir, 'google', file)
        else
          File.join(ENV['HOME'], '.config', 'google', file)
        end
      end

      # Returns application credentials for the given scope.
      def application_credentials_for(scope)
        Google::Auth.get_application_default(scope)
      end

      # Returns user credentials for the given scope. Requests authorization
      # if required.
      def user_credentials_for(scope)
        FileUtils.mkdir_p(File.dirname(token_store_path))

        if ENV['GOOGLE_CLIENT_ID']
          client_id = Google::Auth::ClientId.new(ENV['GOOGLE_CLIENT_ID'], ENV['GOOGLE_CLIENT_SECRET'])
        else
          client_id = Google::Auth::ClientId.from_file(client_secrets_path)
        end
        token_store = Google::Auth::Stores::FileTokenStore.new(:file => token_store_path)
        authorizer = Google::Auth::UserAuthorizer.new(client_id, scope, token_store)

        user_id = 'default'

        credentials = authorizer.get_credentials(user_id)
        if credentials.nil?
          url = authorizer.get_authorization_url(base_url: OOB_URI)
          say "Open the following URL in your browser and authorize the application."
          say url
          code = ask "Enter the authorization code:"
          credentials = authorizer.get_and_store_credentials_from_code(
            user_id: user_id, code: code, base_url: OOB_URI)
        end
        credentials
      end

      # Gets the API key of the client
      def api_key
        ENV['GOOGLE_API_KEY']
      end
    end
  end
end
