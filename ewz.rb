require 'java'

java_import 'com.stocksimulator.main.RubyBSAdapter'
java_import 'com.stocksimulator.main.RBSFactory'
java_import 'com.stocksimulator.abs.Parameters'
java_import 'com.stocksimulator.common_strategies.RubyDoubleRatioAdapter'

class Variables
	def self.symbolA 
		"WINc1"
	end
end
class Start < RBSFactory


	def self.setVar()
		papers = ["EWZ", "DOLc1", Variables.symbolA] #INDc1
		self.setOutputName("ewzFinoNoDup".to_java)
		self.setMongoOutputSymbol(Variables.symbolA)
		self.setLog(true)
		cleanSymbols()
		for paper in papers
			pushSymbol(paper)
		end
	end
	
	def self.run()
		#
		dates = ["14/03/2014","17/03/2014","18/03/2014","19/03/2014", "21/01/2014", "22/01/2014", "23/01/2014", "24/01/2014", "27/01/2014", "28/01/2014","29/01/2014", "30/01/2014","06/02/2014", "05/02/2014", "27/02/2014", "04/02/2014", "26/02/2014", "20/02/2014", "13/02/2014", "10/02/2014", "31/01/2014", "21/02/2014","19/02/2014"]
		#dates = ["14/03/2014"]
		#puts dates
		#dates = []
		#dates = ["19/03/2014"]
		self.setVar()

		ret = []
		for date in dates
			getFile(date)
			waitForFiles()
			ret.push(RubyConf.new("", date))
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
	attr_reader :from, :to, :name,  :bookOrder, :actorsQtd, :strategyType, :watchSymbol, :rbFilename, :rbKlass, :replace

	def to_int(a)
		a.to_java(:int)
	end

	def to_double(b)
		b.to_java(:double)
	end
	
	def initialize(filename, date)
		
		@from = "09:10:00"
		@to = "17:00:00"
		@name = "EWZ-Remote"
		@dbLookUp = "EWZ2" #dbLookupName
		@dateRB = date
		@bookOrder = to_int(30)
		@actorsQtd = to_int(2)
		@replace = false
		@strategyType = "RubyDoubleRatioAdapter"
		@watchSymbol = ["EWZ", "DOLc1"]
		@rbFilename = "self"
		@rbKlass = "RubyStrategy" 
	end
	
	def postRun()
		#pushExcludedHour("10:00:00", "14:10:00")
	end
	
	def mConfig
		mongoConfigMaker("localhost", 27017, @dbLookUp+@dateRB, myFilename)
	end 

	def myInst
		instMaker(["EWZ", "DOLc1", Variables.symbolA])
	end


	def varParam()
		params = []
		elapsed_range = (1500..1500).step(100) #50..500 #50..70
		spread_range = (27..33).step(1) #20..200
		spread_max_range = (0..10).step(1)
		spread_min_range = (40..60).step(1)
		flags = ["entrada_saida"]
		#flags = ["entrada", "saida", "entrada_saida", "off"]
		for preElapsed in elapsed_range
			for spread in spread_range
				for spread_max in spread_max_range
					for spread_min in spread_min_range
					for flag in flags
						a = Parameters.new
						a.set("elapsed", to_int(preElapsed*1000))
						a.set("spread", to_int(spread))
						if flag == "entrada" or flag == "entrada_saida"
							a.set("spread_max", to_int((spread_max/100.0) * spread + spread))
						else
							a.set("spread_max", to_int(0))
						end
						if flag == "saida" or flag == "entrada_saida"
							a.set("spread_min", to_int(spread - (spread_min/100.0) * spread))
						else
							a.set("spread_min", to_int(0))
						end
						a.set("flag", flag)
						params.push(a)
					end
					end
				end
			end	
		end
		params
	end
end

class RubyStrategy < RubyDoubleRatioAdapter
	attr_reader :symbolA, :symbolB, :symbolC, :gran, :maxPos, :step
	def to_int(a)
		a.to_java(:int)
	end

	def initialize(strategy)
		@symbolA = Variables.symbolA
		@symbolB = "EWZ"
		@symbolC = "DOLc1"
		@gran = to_int(5)
		@maxPos = to_int(15)
		@step = to_int(5)
	end

end