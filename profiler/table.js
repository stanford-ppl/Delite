
// rowData = [["Col 1", "Col 2"], ["Row 1 Col 1", "Row 1 Col 2"]]
function createTable(id, parentId, rowData) {
	var table = d3.select(parentId).append("table").attr("id", id)

	table.selectAll('.tHead').data([0]).enter().append('thead').attr("class", "tHead");
	var thead = table.select('thead');

	table.selectAll('.tbody').data([0]).enter().append('tbody').attr("class", "tBody");
	var tbody = table.select('tbody');

	var columnNames = rowData[0]
	var headers = thead.selectAll(".header")
		.data(columnNames)
		.enter().append("td")
		.attr("class", "header")
		.text(function (d) {return d})

	// create a row for each object in the data
	var rows = tbody.selectAll(".tr")
		.data(rowData.slice(1, rowData.length))
		.enter()
		.append("tr")
		.attr("class", "tr");

	// create a cell in each row for each column
	var cells = rows.selectAll(".td")
		.data(function(row) {return row})
		.enter()
		.append("td")
		.attr("class", "td")
	  	.text(function(d) { return d});

	return table;
}