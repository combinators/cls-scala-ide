/*
 * Copyright 2018 Anna Vasileva
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

require(['bootstrap', 'cytoscape'], function(bootstrap, cytoscape) {

  $(function() {
     // Ajax to data

     $.get("graph", function(data){
        console.log("got graph");
        console.log(data);

         try {
         var graph = JSON.parse(data)
         mkGraph(graph, "#cy-graph");
        }
     catch{
        $("#cy-graph").html(data.replace(/\n/g, '<br />'));
        }
     });

  var stepsNr = 0;
  $(function(){
    $(".nav-sidebar a").click(function(){
        $(this).tab('show');
        });
        $('.nav-sidebar a[href="#steps"]').on('shown.bs.tab', function(){
            mkSteps(stepsNr);
            $.get("showUninhabitedTy", function(data){
                if(!data){
                    $("#unusableTy").addClass('hidden');
                }
               $("#debuggerMessages").html(data.replace(/\n/g, '<br />'));

            });
            $.get("showUnusableCMsg", function(data){
                if(!data){
                    $("#unusableCom").addClass('hidden');
                }
               $("#debuggerM").html(data.replace(/\n/g, '<br />'));
            });
            $.get("showUnusableBecauseOfTy", function(data){
                if(!data){
                    $("#unusableSub").addClass('hidden');
                }
                $("#deM").html(data.replace(/\n/g, '<br />'));
            });
        });

        $('.nav-sidebar a[href="#repo"]').on('shown.bs.tab', function(){
            $.get("repository", function(data){
              $("#repository").html(data.replace(/\n/g, '<br />'));
               });
        });

        $('.nav-sidebar a[href="#mess"]').on('shown.bs.tab', function(){
            $.get("showDebuggerMessages", function(data){
             $("#showDebuggerMessages").html(data.replace(/\n/g, '<br />'));
            });
        });

        $(".collapse").collapse('hide');
            $("#collapse").on("shown.bs.collapse", function(){
             });


});
   $('#forwardButton, #backwardButton, #toggleCyclesButton').click(function(){

        if(this.id == 'toggleCyclesButton'){
                toggleCycle(stepsNr);
                }else{

        if(this.id == 'forwardButton'){
            stepsNr ++;
        }else{
            if(this.id == 'backwardButton'){
            stepsNr --;
            }
        }

        mkSteps(stepsNr);
        }

   });

   function toggleCycle(stepNr){
        $.getJSON("toggleCycle/" + stepsNr, function(data){
        mkGraph(data, "#cy-steps")
                            });
                   }

   });


    $("#submit").click(function(){
        var x = document.getElementById("request").value;
        computeNewRequest(x);
    });


    function computeNewRequest(request) {
    if (request.includes("[")){
    console.log("JA", request.replace(/\[/g, '&#91;'))
    }else {
    console.log("Nein", request)
    } var req = request.replace(/\[/g, '91')
    req = req.replace(/\]/g, '93')
    console.log("req", req)
        $.get("computeRequest/" + req, function(data){
            $.get("graph", function(data){
                    console.log("req", req)
                     var graph = JSON.parse(data)
                     $("#cy-graph").html(" ");
                     mkGraph(graph, "#cy-graph");
            });
        });
    }

      function mkSteps(stepsNr) {
        console.log("Step", parseInt(stepsNr));
            $.getJSON("steps/" + stepsNr, function(data){
                mkGraph(data, "#cy-steps")
            });
        }


    function mkGraph(graph, canvas){
      // Setup basic graph
      var cy = cytoscape({
                   container: $(canvas),
                   boxSelectionEnabled: false,
                   autounselectify: true,

                   style: [
                     {
                        selector: 'node',
                        css: {
                          'content': 'data(label)'
                        }
                     },
                     {
                        selector: 'node[style = "type-node"]',
                        css: {
                          'padding-top': '10px',
                          'padding-left': '10px',
                          'padding-bottom': '10px',
                          'padding-right': '10px',
                          'text-valign': 'top',
                          'text-halign': 'center',
                          'background-color': '#FFB147',
                          'shape' : 'roundrectangle'

                        }
                     },
                     {
                        selector: 'node[style = "target-node"]',
                        css: {
                          'padding-top': '10px',
                          'padding-left': '10px',
                          'padding-bottom': '10px',
                          'padding-right': '10px',
                          'text-valign': 'top',
                          'text-halign': 'center',
                          'background-color': '#3BD579',
                          'shape' : 'roundrectangle'
                        }
                     },

                     {
                         selector: 'node[style = "uninhabited-type-node"]',
                         css: {
                            'visibility': 'hidden',
                            'padding-top': '10px',
                            'padding-left': '10px',
                            'padding-bottom': '10px',
                            'padding-right': '10px',
                            'text-valign': 'top',
                            'text-halign': 'center',
                            'background-color': '#3bc8d5',
                            'shape' : 'roundrectangle'
                         }
                      },
                     {
                       selector: 'node[style = "combinator-node"]',
                       css: {
                         'text-valign': 'top',
                         'text-halign': 'center',
                         'padding-top': '3em',
                         'padding-left': '10px',
                         'padding-bottom': '10px',
                         'padding-right': '10px',
                         'background-color': '#428CCA',
                         'shape' : 'ellipse'
                       }
                     },
                     {
                        selector: 'node[style = "unusable-combinator-node"]',
                        css: {
                          'text-valign': 'top',
                          'text-halign': 'center',
                          'padding-top': '3em',
                          'padding-left': '10px',
                          'padding-bottom': '10px',
                          'padding-right': '10px',
                          'background-color': '#FF3100',
                          'shape' : 'ellipse'
                        }
                      },
                      {
                         selector: 'node[style = "unvisible-unusable-combinator-node"]',
                         css: {
                           'visibility': 'hidden',
                           'text-valign': 'top',
                           'text-halign': 'center',
                           'padding-top': '3em',
                           'padding-left': '10px',
                           'padding-bottom': '10px',
                           'padding-right': '10px',
                           'background-color': '#FF3100',
                           'shape' : 'ellipse'
                         }
                     },
                     {
                       selector: 'node[style = "argument-node"]',
                       css: {
                         'text-valign': 'center',
                         'text-halign': 'center',
                         'background-color': '#FF9400',
                         'shape' : 'octagon'
                       }
                     },

                     {
                       selector: 'edge',
                       css: {
                         'source-label': 'data(label)',
                         'source-text-margin-y': -20,
                         'source-text-margin-x': -20,
                         'source-text-offset': '90',
                         'target-arrow-shape': 'triangle',
                         'curve-style': 'bezier',
                         'line-color': 'black',
                         'target-arrow-color': 'black',
                         'source-arrow-color': 'black'
                       }
                     },
                     {
                       selector: ':selected',
                       css: {
                         'background-color': 'black',
                         'line-color': 'black',
                         'target-arrow-color': 'black',
                         'source-arrow-color': 'black'
                       }
                     }
                   ],

                   elements: {
                     nodes: graph.nodes,
                     edges: graph.edges
                   },

                 });

                 var layoutName = 'breadthfirst';

                 // Callback for relayout
                 var adjustLayout = function () {
                    var cyDiv = $(canvas);
                   var main = $(window);
                    var delta = cyDiv.outerHeight(true) - cyDiv.innerHeight();
                    var parents = cyDiv.parentsUntil(main);
                    $.each(parents, function (pos, value) { delta = delta + $(value).outerHeight(true) - $(value).innerHeight(); });

                    cyDiv.height(main.height() - cyDiv.offset().top - delta - 20);

                    cy.resize();
                    if (layoutName != 'custom') {
                     cy.filter('node[style != "type-node"]').layout({
                        name: 'cose',
                        condense: true,
                        fit: false,
                        padding: 75,
                        avoidOverlapPadding: 75,
                        ready: function() { console.log("resized"); }
                     }).run();}

                     cy.layout({
                        name: layoutName,
                        boundingBox: { x1: 0, y1: 0, w: cyDiv.width(), h: cyDiv.height()},
                        padding: 30,
                        animate: true,
                        avoidOverlap: true,
                        avoidOverlapPadding: 75,
                        ready: function()  {console.log("bf resized") } }).run()
                 }

                 adjustLayout();

                 // Adjust layout on window size change
                 window.resizeEvt;

                 $(window).resize(function() {
                    clearTimeout(window.resizeEvt);
                    window.resizeEvt = setTimeout(adjustLayout, 250);
                 });

                 // React on layout dropbox
                 $('#layout-dropdown').find('.dropdown-menu a').on('click', function () {
                    $('#layout-dropdown').find('button').text($(this).text()).append(' <span class="caret"></span>');
                    layoutName = $(this).text().toLowerCase();
                    adjustLayout();
                 });

                 cy.center();
                 // Change layout to custom when dragging and releasing any node
                 cy.nodes().forEach(function(node) {
                    node.on("free", function (e) {
                        layoutName = 'Custom';
                        $('#layout-dropdown').find('button').text(layoutName).append(' <span class="caret"></span>');
                    });

                 });

                 cy.filter('node[style = "unusable-combinator-node"]').on('tap', function(event){
                    var node = event.target._private.data.label
                    $.get('showPosition/' + node, function(data){
                         $('#showUnusablePosition').html(data);});
                 });

                 cy.filter('node[style = "combinator-node"]').on('tap', function(event){
                    var node = event.target._private.data.label
                        $.get('showPosition/' + node, function(data){
                          $('#showPosition').html(data);});
                        });


                cy.nodes().on('click', function (evt) {
                    var node = evt.target._private.data.label
                    $.get('showPosition/' + node, function(data){
                        $("#position1").text(data);
                    });
               });

        }
});
