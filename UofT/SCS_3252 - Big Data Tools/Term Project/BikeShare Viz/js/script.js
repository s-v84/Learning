//1. Define listeners and other variables.
//1.a. listeners 

$('.slower').click(function(){
    if(timeFactor > 1){
        timeFactor -= 1;  
    };

    $('.timeFactor').html(timeFactor);

});

$('.faster').click(function(){
    timeFactor += 1;
    $('.timeFactor').html(timeFactor);

});

//1.b. Variables 
var timeFactor = 5; //number of minutes in real life to a second in the visualization
$('.timeFactor').html(timeFactor); //Displays the timeFactor in the user interface.
var timer;
var time = moment.unix(currentIterationTime);
var currentIterationTime = dayStartTime - 60;
var runningMemberTrips = 0;
var runningCasualTrips = 0;
      
//Add from_tally and to_tally properties to each station in the list.
//initialized to zero.
for(var i = 0; i < stationLocations.features.length; i++){ 
  stationLocations.features[i].properties.from_tally=0;
  stationLocations.features[i].properties.to_tally=0;
}


/***
 * Updates the clock in the user interface to dislplay the current value of time
 * and date within the visualization
 */
function updateTimer() {
    //time.add('minutes',1);
    $('.readableTime').text(time.format('h:mm a'));
    $('.date').text(time.format('dddd, MMMM Do YYYY'));
    timer = setTimeout(function(){updateTimer()},(1000/timeFactor));
}

    
//Initialize map
var tiles = L.tileLayer('http://tile.stamen.com/toner/{z}/{x}/{y}.png', {
    attribution: '<a href="" target="_blank">Terms &amp; Feedback</a>'
});

var map = L.map('map',{ zoomControl:false })
    .addLayer(tiles)
    .setView([43.6532, -79.3832], 13);

//Add SVG overlay    
var svg = d3.select(map.getPanes().overlayPane).append("svg");
var g = svg.append("g").attr("class", "leaflet-zoom-hide")
    
   
//Read in the stations file and plot as points on the map.
var geojsonMarkerOptions = {
    radius: 3,
    fillColor: "#ff7800",
    color: "#000",
    weight: 1,
    opacity: 1,
    fillOpacity: 0.8
};
L.geoJSON(stationLocations, {
    pointToLayer: function (feature, latlng) {
        return L.circleMarker(latlng, geojsonMarkerOptions);
    }
}).addTo(map);


/**
 * A function that converts a lat long point to a point on the SVG
 * layer.
 **/
function projectPoint(x, y) {
  var point = map.latLngToLayerPoint(new L.LatLng(y, x));
  this.stream.point(point.x, point.y);
}

var transform = d3.geo.transform({point: projectPoint});
var path = d3.geo.path().projection(transform);

// For each trip add an SVG path object with linear interpolation
// as we only have the start and end destination and aim to plot
// only a simple Euclidean path.      
// Also add a 'class' attribute with the following format path.trip<tripID>
// for easy retrival later on.  
var feature = g.selectAll("path")
        .data(bicycleTrips.features)
        .enter()
        .append("path")
        .attr("class",function(d){
          return "trip" + d.properties.trip_id;
        })
        .attr("style", "opacity:0")
        .attr("d", d3.svg.line()
        .interpolate("linear"));
  
map.on("viewreset", reset);
reset();
  
/** 
 * A function that repositions the SVG layer to cover the features.
 **/
function reset() {
  var bounds = path.bounds(bicycleTrips),
      topLeft = bounds[0],
      bottomRight = bounds[1];

  svg .attr("width", bottomRight[0] - topLeft[0])
      .attr("height", bottomRight[1] - topLeft[1])
      .style("left", topLeft[0] + "px")
      .style("top", topLeft[1] + "px");

  g   .attr("transform", "translate(" + -topLeft[0] + "," + -topLeft[1] + ")");

  feature.attr("d", path);
}     


/**
 * The entry function to begin displaying the animation.
 * Iterates from day start time to day end time in increments of one minute each,
 * and fetches all trips that started at that minute and passes them to the 
 * animatePath() function to perform the animation.
 **/
function displayAnimation(){  
      currentIterationTime = currentIterationTime + 60;
      time = moment.unix(currentIterationTime);
      
      var filterEndTime = currentIterationTime + 60;
      var objs = bicycleTrips.features.filter(a=> ((a.properties.trip_start_time_unix >= currentIterationTime) && (a.properties.trip_start_time_unix < filterEndTime)))
      animatePath(objs);
      
     //if there are more minutes left in the day, the funcion calls itself
     //again.
     if(currentIterationTime < dayEndTime){
        setTimeout(displayAnimation, 1000/timeFactor);
     }
     
}

/**
 * Takes an array of geoJson bicycle trip objects, iterates though them,
 * fetches the SVG path for each trip, displays it by changing opacity,
 * appends a new circle (red for members, blue for casual users) and passes
 * this circle and path to the transitionCircle function for animation along the path.
 **/ 
function animatePath(objs){
   for(var i = 0; i < objs.length; i++){
     var nextPath = svg.select("path.trip"+objs[i].properties.trip_id);
     nextPath.attr({"stroke": "grey"})
     .attr("style", "opacity:0.7");
     var startPoint =  pathStartPoint(nextPath);
     
     var fillColor;
     if(objs[i].properties.user_type == "Member"){
        fillColor = "red";
     }
     else{
        fillColor = "steelblue";
     }
     var circle = g.append("circle")
                  .attr("r", 4)
                  .attr("fill", fillColor)
                  .attr("transform", "translate(" + startPoint[0] + "," + startPoint[1] + ")");
     transitionCircle(circle, nextPath, objs[i]);
   }
}
    
/**
 * The overall start point for the entire visualization. 
 * Fades out all overlay layers and fades in all box layers (summary box and timer)
 * and invokes the displayAnimation() function and the updateRunning() function.
 **/
function begin(){
  $('.overlay').fadeOut(250);
  $('.box').fadeIn(250);
  var functionControl = setTimeout(function(){
      updateTimer();
      displayAnimation();
      updateRunning();
  },1000/timeFactor);

}
   
    
/**
 * A function that finds the start point of an SVG path object.
 **/
function pathStartPoint(path) {
    var d = path.attr('d');

    dsplitted = d.split("L")[0].slice(1).split(",");
    var point = []
    point[0]=parseInt(dsplitted[0]);
    point[1]=parseInt(dsplitted[1]);

    return point;
}

/**
 * A function that animates a circle along the supplied path.
 * Also applies a sort of pulsating animation to the circle when it reaches the
 * end of the path by increasing the radius and finally removes it from the SVG layer.
 **/
function transitionCircle(circle, path, tripData) {
  circle.transition()
      .duration(function(){
        var tripDurationMin = (tripData.properties.trip_duration_seconds)/60;
        tripDurationMin = tripDurationMin * (1/timeFactor) * 500;
        return (tripDurationMin + 500/timeFactor);
      })
      .attrTween("transform", translateAlong(path.node()))
      .each("end", function(){
            circle.transition()
        					.duration(500)
        					.attr("stroke-width", 2)
        					.attr("r", 10)
        					.transition()
        					.duration(500)
        					.attr('stroke-width', 0.5)
        					.attr("r", 7)
        					.ease('sine')
                  .each("end", function(){
                      this.remove();
                  });
            path.attr("style", "opacity:0");
            
            //update totals
            if (tripData.properties.user_type == "Casual"){
              runningCasualTrips = runningCasualTrips + 1;
            }
            if (tripData.properties.user_type == "Member"){
              runningMemberTrips = runningMemberTrips + 1;
            }
            
            var fromStationId = tripData.properties.from_station_id;
            var fromStationObj = stationLocations.features.filter(a=> (a.properties.station_id == fromStationId));
            fromStationObj[0].properties.from_tally = fromStationObj[0].properties.from_tally + 1;
            
            var toStationId = tripData.properties.to_station_id;
            var toStationObj = stationLocations.features.filter(a=> (a.properties.station_id == toStationId));
            toStationObj[0].properties.to_tally = toStationObj[0].properties.to_tally + 1; 
            
            setTimeout(updateRunning(), 1000/timeFactor);
      });
}
    
/**
 * A function that returns an attrTween for translating along the specified path element.
 **/
function translateAlong(path) {
  var l = path.getTotalLength();
  return function(d, i, a) {
    return function(t) {
      var p = path.getPointAtLength(t * l);
      return "translate(" + p.x + "," + p.y + ")";
    };
  };
}

/**
 * A function that updates the running totals: total member and casual user trips
 * and the top 3 from and to stations.
 * To find top 3 from stations, the stations geoJSON array is sorted by from_trips_tally
 * in descending order and then extracting the first 3 elements of the sorted array.
 * The top 3 To stations are found in a similar manner using the to_trips_tally property.
 **/
function updateRunning() {
    stationLocations.features.sort(function(a, b) {
        return -(parseFloat(a.properties.from_tally) - parseFloat(b.properties.from_tally));
    });
    var top1FromStation = stationLocations.features[0].properties.name + " [" + stationLocations.features[0].properties.from_tally + "]";
    var top2FromStation = stationLocations.features[1].properties.name + " [" + stationLocations.features[1].properties.from_tally + "]";;
    var top3FromStation = stationLocations.features[2].properties.name + " [" + stationLocations.features[2].properties.from_tally + "]";;
    
    stationLocations.features.sort(function(a, b) {
        return -(parseFloat(a.properties.to_tally) - parseFloat(b.properties.to_tally));
    });
    var top1ToStation = stationLocations.features[0].properties.name + " [" + stationLocations.features[0].properties.to_tally + "]";
    var top2ToStation = stationLocations.features[1].properties.name + " [" + stationLocations.features[1].properties.to_tally + "]";
    var top3ToStation = stationLocations.features[2].properties.name + " [" + stationLocations.features[2].properties.to_tally + "]";
    
    $('.readableTime').text(time.format('h:mm a'));
    $('.runningMemberTrips').text(runningMemberTrips);
    $('.runningCasualTrips').text(runningCasualTrips);
    $('.runningTotalTrips').text(runningMemberTrips + runningCasualTrips);
    
    $('.runningTop1FromStation').text(top1FromStation);
    $('.runningTop2FromStation').text(top2FromStation);
    $('.runningTop3FromStation').text(top3FromStation);
    
    $('.runningTop1ToStation').text(top1ToStation);
    $('.runningTop2ToStation').text(top2ToStation);
    $('.runningTop3ToStation').text(top3ToStation);
}