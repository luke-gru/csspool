module CSSPool
  module Terms
    class Rgb < CSSPool::Node
      attr_accessor :red
      attr_accessor :green
      attr_accessor :blue
      attr_accessor :percentage
      attr_accessor :parse_location
      attr_accessor :operator
      alias :percentage? :percentage

      def initialize red, green, blue, percentage, operator, parse_location
        super()
        @red    = red
        @green  = green
        @blue   = blue
        @percentage = percentage
        @operator = operator
        @parse_location = parse_location
      end
    end
  end
end
