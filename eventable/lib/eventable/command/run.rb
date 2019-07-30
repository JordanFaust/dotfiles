# require 'open3'
require 'dbus'

module Eventable
  module Command
    class Run
      def self.execute()
        bus = DBus.session_bus
        service = bus.request_service("org.eventable.service")

        # Setup Calendar Interface
        calendar_worker = Eventable::Worker::Calendar.new({ threads: 5 })
        calendar = Eventable::Interface::Calendar.new("/org/eventable/Calendar", calendar_worker)
        # Export the service on the bus
        service.export(calendar)
        # Define the change handler
        calendar_worker.on_change do
          calendar.PropertiesChanged()
        end
        # Use the current set date to refresh the data at the specified interval
        # calendar_worker.refresh({ interval: 30 }) do |w|
        #   start_time, end_time = w.store[:time_range]
        #   w.client.list(start_time, end_time)
        # end

        # Start bus loop
        loop = DBus::Main.new
        loop << bus
        loop.run
      end
    end
  end
end
