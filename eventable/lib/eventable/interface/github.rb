require 'dbus'

module Eventable
  module Interface
    class Github < DBus::Object

      def initialize(path, logger)
        super(path)
        @worker = Eventable::Worker::Pool.new({ threads: 5 })
        @logger = logger
        @client = Eventable::Clients::Github.new()
        @interface = "org.eventable.GithubInterface"
        setup_handlers
      end

      dbus_interface "org.eventable.GithubInterface" do
        # Change the date that calendar events are returned for
        dbus_method :Refresh do
          @worker.process do
            @logger.info("[#{@interface}] triggering refresh")
            @worker.store[:metadata] = @client.list()
          end
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

      def setup_handlers()
        @worker.process do
          @logger.info("[#{@interface}] seeding initial data")
          @worker.store[:metadata] = @client.list()
        end
        @worker.on_change do
          @logger.info("[#{@interface}] sending PropertiesChanged signal")
          self.PropertiesChanged()
        end
        @worker.refresh({ interval: 300 }) do
          @logger.info("[#{@interface}] refreshing at interval=300")
          @worker.store[:metadata] = @client.list()
        end
      end
    end
  end
end
