require 'dbus'

module Eventable
  module Interface
    class Calendar < DBus::Object

      def initialize(path, worker)
        super(path)
        @worker = worker
        @client = Eventable::Clients::Calendar.new()
        @store = {}
      end

      dbus_interface "org.eventable.CalendarInterface" do
        # Change the date that calendar events are returned for
        dbus_method :ChangeDate, "in start:s, in end:s" do |start_time, end_time|
          @worker.process do
            value = @client.list({start_time: start_time, end_time: end_time})
            @store[:metadata] = value
            value
          end
        end

        # Returns meetings within the current date range
        # dbus_method :metadata, "out ret:aa{sv}" do
        dbus_method :Metadata, "out ret:aa{sv}" do
          return [@store[:metadata] || []]
        end

        # Triggered when the Eventable::Store::Calendar.meetings data is updated
        dbus_signal :PropertiesChanged
      end
    end
  end
end
