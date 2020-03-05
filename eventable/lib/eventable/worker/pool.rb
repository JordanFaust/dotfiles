require 'concurrent'
require 'logger'

module Eventable
  module Worker
    class Pool
      attr_accessor :store

      def initialize(params = {})
        @pool = Concurrent::FixedThreadPool.new(params[:threads] || 5)
        @store = Concurrent::Map.new()
        @task = nil
        @signal = Proc.new {}
        @callback = Proc.new {}
      end

      # Process the work item and signal when complete
      def process(&block)
        # Concurrent work
        @pool.post do
          @callbock ||= block
          properties = block.call()
          @signal.call(properties)
        end
      end

      # Define the block that is executed when a work item is complete
      def on_change(&block)
        @signal = block
      end

      # Sets the interval at which data is refreshed.
      def refresh(params, &block)
        @task = Concurrent::TimerTask.new(execution_interval: params[:interval] || 10) do
          block.call
        end
        @task.execute
      end
    end
  end
end
