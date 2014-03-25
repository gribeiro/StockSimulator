require 'java'

java_import 'com.stocksimulator.main.RubyBSAdapter'
java_import 'com.stocksimulator.main.RBSFactory'
java_import 'com.stocksimulator.abs.Parameters'
java_import 'com.stocksimulator.common_strategies.RubyRatioAdapter'

class Variables
	def self.symbolA 
		"INDc1"
	end
end
class Start < RBSFactory


	def self.setVar()
		papers = ["ISPc1", Variables.symbolA] #INDc1
		self.setOutputName("spWithTimeExit".to_java)
		self.setMongoOutputSymbol(Variables.symbolA)
		self.setLog(true)
		cleanSymbols()
		for paper in papers
			pushSymbol(paper)
		end
	end
	
	def self.run()
		#
		dates = ["21/01/2014", "22/01/2014", "23/01/2014", "24/01/2014", "27/01/2014", "28/01/2014","29/01/2014", "30/01/2014","06/02/2014", "05/02/2014", "27/02/2014", "04/02/2014", "26/02/2014", "20/02/2014", "13/02/2014", "10/02/2014", "31/01/2014", "21/02/2014","19/02/2014", "18/02/2014", "03/02/2014",  "07/02/2014",  "11/02/2014", "12/02/2014",  "14/02/2014", "25/02/2014", "06/03/2014", "07/03/2014", "10/03/2014", "11/03/2014", "12/03/2014", "13/03/2014"]
		#puts dates
		#dates = ["11/03/2014"]
		#dates = []
		self.setVar()

		ret = []
		for date in dates
			alloc = getFile(date)
			puts alloc
			ret.push(RubyConf.new(alloc, date))
		end
		ret.to_java RubyBSAdapter
	end

end

class RubyConfFactory
	def self.getNew(filename, date)
		RubyConf.new(filename, date)
	end
end

class RubyConf < RubyBSAdapter
	attr_reader :from, :to, :name,  :bookOrder, :actorsQtd, :strategyType, :watchSymbol, :rbFilename, :rbKlass

	def to_int(a)
		a.to_java(:int)
	end

	def initialize(filename, date)
		
		@from = "09:00:00"
		@to = "17:40:00"
		@name = "sp500-Remote"
		@dbLookUp = "sp500" #dbLookupName
		@dateRB = date
		@bookOrder = to_int(10)
		@actorsQtd = to_int(3)
		@strategyType = "RubyRatioAdapter"
		@watchSymbol = "ISPc1"
		@rbFilename = "self"
		@rbKlass = "RubyStrategy" 
	end
	
	def postRun()
		#pushExcludedHour("10:00:00", "14:10:00")
	end
	
	def mConfig
		mongoConfigMaker("192.168.90.15", 27017, @dbLookUp+@dateRB, myFilename)
	end 

	def myInst
		instMaker(["ISPc1", Variables.symbolA])
	end

	def varParam()
		params = []
		elapsed_range = (50..500).step(50) #50..500 #50..70
		spread_entrada_range = (50..150).step(25) #20..200
		#saida_range = (100..100).step(10) #0..spread_max_range
		spread_max_range = (0..20).step(20)
		time_exit_range = (15..60).step(15)
		for preElapsed in elapsed_range
			for entrada in spread_entrada_range
				for spread_max in spread_max_range
					for time_exit in time_exit_range
						a = Parameters.new
						a.set("elapsed", to_int(preElapsed*1000))
						a.set("spread_entrada", to_int(entrada))
						a.set("spread_max", to_int((spread_max/100.0) * entrada + entrada))
						a.set("time_exit", to_int(time_exit*60))
						params.push(a)
					end
				end
			end	
		end
		params
	end
end

class RubyStrategy < RubyRatioAdapter
	attr_reader :symbolA, :symbolB, :gran, :maxPos, :step
	def to_int(a)
		a.to_java(:int)
	end

	def initialize(strategy)
		@symbolA = Variables.symbolA
		@symbolB = "ISPc1"
		@gran = to_int(5)
		@maxPos = to_int(15)
		@step = to_int(5)
	end

end