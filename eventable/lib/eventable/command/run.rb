# require 'open3'
require 'dbus'

module Eventable
  module Command
    class Run
      def self.execute()
        logger = Logger.new(STDOUT)
        logger.formatter = proc do |severity, datetime, _, message|
          "#{Time.parse(datetime.to_s).utc.iso8601}: #{message}\n"
        end

        bus = DBus.session_bus
        service = bus.request_service("org.eventable.service")

        calendar = Eventable::Interface::Calendar.new("/org/eventable/Calendar", logger)
        # Export the service on the bus
        service.export(calendar)

        github = Eventable::Interface::Github.new("/org/eventable/Github", logger)
        service.export(github)

        # Start bus loop
        loop = DBus::Main.new
        loop << bus
        loop.run
      end
    end
  end
end
