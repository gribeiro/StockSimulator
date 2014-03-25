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
		self.setOutputName("ewzSemLeilao".to_java)
		self.setMongoOutputSymbol(Variables.symbolA)
		self.setLog(true)
		cleanSymbols()
		for paper in papers
			pushSymbol(paper)
		end
	end
	
	def self.run()
		#
		#dates = ["21/01/2014", "22/01/2014", "23/01/2014", "24/01/2014", "27/01/2014", "28/01/2014","29/01/2014", "30/01/2014","06/02/2014", "05/02/2014", "27/02/2014", "04/02/2014", "26/02/2014", "20/02/2014", "13/02/2014", "10/02/2014", "31/01/2014", "21/02/2014","19/02/2014", "18/02/2014", "03/02/2014",  "07/02/2014",  "11/02/2014", "12/02/2014",  "14/02/2014", "25/02/2014", "06/03/2014", "07/03/2014", "10/03/2014", "11/03/2014", "12/03/2014", "13/03/2014"]
		#puts dates
		dates = ["19/03/2014"]
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
	attr_reader :from, :to, :name,  :bookOrder, :actorsQtd, :strategyType, :watchSymbol, :rbFilename, :rbKlass, :replace

	def to_int(a)
		a.to_java(:int)
	end

	def to_double(b)
		b.to_java(:double)
	end
	
	def initialize(filename, date)
		
		@from = "10:00:00"
		@to = "17:00:00"
		@name = "EWZ-Remote"
		@dbLookUp = "EWZ2" #dbLookupName
		@dateRB = date
		@bookOrder = to_int(30)
		@actorsQtd = to_int(4)
		@replace = true
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
		elapsed_range = (100..500).step(100) #50..500 #50..70
		spread_entrada_range = (20..100).step(20) #20..200
		spread_max_range = (0..50).step(25)
		#for preElapsed in elapsed_range
			for entrada in spread_entrada_range
				#for spread_max in spread_max_range
				
						a = Parameters.new
						#a.set("elapsed", to_int(preElapsed*1000))
						a.set("spread_entrada", to_int(entrada))
						a.set("ratio", to_double(0.48))
						#a.set("spread_max", to_int((spread_max/100.0) * entrada + entrada))
				
						params.push(a)
			
				#end
			#end	
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