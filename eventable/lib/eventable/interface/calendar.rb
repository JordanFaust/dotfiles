require 'dbus'

module Eventable
  module Interface
    class Calendar < DBus::Object

      def initialize(path, logger)
        super(path)
        @worker = Eventable::Worker::Pool.new({ threads: 5 })
        @logger = logger
        @client = Eventable::Clients::Calendar.new()
        @interface = "org.eventable.CalendarInterface"
        setup_handlers
      end

      dbus_interface "org.eventable.CalendarInterface" do
        # Change the date that calendar events are returned for
        dbus_method :ChangeDate, "in start:s, in end:s" do |start_time, end_time|
          @logger.info("[#{@interface}] changing date: start=#{start_time} end=#{end_time}")
          change_date(start_time, end_time)
        end

        # Returns meetings within the current date range
        # dbus_method :metadata, "out ret:aa{sv}" do
        dbus_method :Metadata, "out ret:aa{sv}" do
          @logger.info("[#{@interface}] getting metadata")
          return [@worker.store[:metadata] || []]
        end

        # Triggered when the Eventable::Store::Calendar.meetings data is updated
        dbus_signal :PropertiesChanged
      end

      private

      def change_date(start_time, end_time)
        @worker.process do
          @worker.store[:date_range] = { start_time: start_time, end_time: end_time }
          value = @client.list({ start_time: start_time, end_time: end_time })
          @worker.store[:metadata] = value
          value
        end
      end

      def setup_handlers()
        @worker.on_change do
          @logger.info("[#{@interface}] sending PropertiesChanged signal")
          self.PropertiesChanged()
        end
        @worker.refresh({ interval: 300 }) do
          @logger.info("[#{@interface}] refreshing at interval=300")
          start_time = @worker.store[:date_range][:start_time]
          end_time = @worker.store[:date_range][:end_time]
          change_date(start_time, end_time)
        end
      end
    end
  end
end
