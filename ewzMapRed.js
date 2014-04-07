var map = function Map() {
	
	
	emit(
                this.inputStr,    // how to group
		{PNL: this.PNL, sharpe: this.sharpe, sortino: this.sortino}	// associated data point (document)
	); 
	
	
};

var reduce = function Reduce(key, values) {
	

	var reduced = {PNL: 0, sharpe: 0, sortino:0}; // initialize a doc (same format as emitted value)

	values.forEach(function(val) {
		reduced.PNL += val.PNL;
		reduced.sharpe += val.sharpe/values.length; 	
                reduced.sortino += val.sortino/values.length;// reduce logic
	});

	return reduced;	
	
};

var out = {out: "EWZ-TESTE"}
db.georgesTeste.mapReduce(map, reduce, out)