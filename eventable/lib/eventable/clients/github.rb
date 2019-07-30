require 'octokit'

module Eventable
  module Clients
    class Github

      def list()
        Octokit.configure do |c|
          c.api_endpoint = ENV["GITHUB_API_ENDPOINT"] || "https://github.bamtech.co/api/v3/"
          c.access_token = ENV["GITHUB_ACCESS_TOKEN"]
        end

        client = Octokit::Client.new

        issues = client.search_issues("is:open is:pr review-requested:jfaust")

        pull_requests = {}
        issues[:items].each do |pr|
          title = pr[:title]
          comments = pr[:comments]
          number = pr[:number]
          url = pr[:html_url]
          updated_at = pr[:updated_at]

          pull_request = {
            "title" => title,
            "number" => number,
            "comments" => comments,
            "url" => url,
            "updated_at" => updated_at
          }

          pull_requests << pull_request
        end

        pull_requests
      end
    end
  end
end
