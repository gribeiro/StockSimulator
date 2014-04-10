require 'java'

java_import 'com.stocksimulator.main.RubyBSAdapter'
java_import 'com.stocksimulator.main.RBSFactory'
java_import 'com.stocksimulator.abs.Parameters'
java_import 'com.stocksimulator.common_strategies.RubyOptionAdapter'

class Variables
	def self.symbolA 
		"PETR4.SA"
	end

	def self.symbolB
		"PETR4$16"
	end
end
class Start < RBSFactory


	def self.setVar()
		papers = [Variables.symbolB, Variables.symbolA] #INDc1
		self.setOutputName("PETROption".to_java)
		self.setLog(true)
		cleanSymbols()
		for paper in papers
			pushSymbol(paper)
		end
	end
	
	def self.run()
		#dates = []
		dates = ["02/04/2014"]
		self.setVar()

		ret = []
		for date in dates
			file = getFile(date)
			waitForFiles()
			ret.push(RubyConf.new(file, date))
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
	attr_reader :from, :to, :name,  :bookOrder, :actorsQtd, :strategyType, :watchSymbol, :rbFilename, :rbKlass, :replace, :javaFilename

	def to_int(a)
		a.to_java(:int)
	end

	def to_double(b)
		b.to_java(:double)
	end
	
	def initialize(filename, date)
		
		@from = "10:10:00"
		@to = "17:00:00"
		@name = "GeorgesJob"
		@dbLookUp = "PETROption2"
		@dateRB = date
		@bookOrder = to_int(30)
		@actorsQtd = to_int(2)
		@replace = false
		@strategyType = "RubyOptionAdapter"
		@watchSymbol = [Variables.symbolA]
		@javaFilename = ""
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
		instMaker([Variables.symbolA, Variables.symbolB])
	end


	def varParam()
		addParam(50.0,200.0,50.0,"elapsed")
		addParam(0.0, 0.2, 0.02, "spread")
		generateParams
	end
end

class RubyStrategy < RubyOptionAdapter
	attr_reader :symbolA, :symbolB, :symbolC, :gran, :maxPos, :step
	def to_int(a)
		a.to_java(:int)
	end

	def initialize(strategy)
		@symbolA = Variables.symbolA
		@symbolB = Variables.symbolB
		@gran = 0.01
		@maxPos = to_int(15)
		@step = to_int(100)
	end

end