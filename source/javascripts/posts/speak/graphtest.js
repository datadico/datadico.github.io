var svg = d3.select("#graphtest").append("svg").attr("width", 100).attr("height", 100);

svg.append("circle").attr("r", 20).attr({
  cx: 40,
  cy: 40
}).style("fill", "red");
