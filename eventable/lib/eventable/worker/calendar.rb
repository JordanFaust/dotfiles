require 'concurrent'

module Eventable
  module Worker
    class Calendar
      def initialize(params = {})
        @pool = Concurrent::FixedThreadPool.new(params[:threads] || 5)
        @signal = Proc.new {}
      end

      # Process the work item and signal when complete
      def process
        # Concurrent work
        @pool.post do
          properties = yield
          @signal.call(properties)
        end
      end

      # Define the block that is executed when a work item is complete
      def on_change(&block)
        @signal = block
      end
    end
  end
end
