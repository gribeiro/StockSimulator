require 'java'

java_import 'com.stocksimulator.common_strategies.QuoteOnlyStrategy'

class RubyStrategy < QuoteOnlyStrategy

  def onQuotes()
     puts "YEAH!!!"
  end 
end