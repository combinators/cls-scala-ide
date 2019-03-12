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

/*    require(['bootstrap', 'cytoscape', 'cytoscape-popper'], function(bootstrap, cytoscape, popper) {
        popper(cytoscape);*/
    loadDoc();
    $(function() {
        loadDoc();

     // Ajax to data
     $.get("graph", function(data){
        $("#progress").html(" ");
        try {
         var graph = JSON.parse(data)
         mkGraph(graph, "#cy-graph");
        }
        catch{
            var news = loadDoc();
            $("#cy-graph").html(data.replace(/\n/g, '<br />'));
        }
     });
   });

   function loadDoc() {
        $.ajax({
         url:"GET",
         async: true,
         xhr: function () {
              var xhr = new window.XMLHttpRequest();
              //Upload Progress
              xhr.addEventListener("progress", function (evt) {
                 if (evt.lengthComputable) {
                var percentComplete = (evt.loaded / evt.total) * 100; $("#dynamic").css({ "width": percentComplete + "%" });
                percentComplete;} }, false);
        return xhr;
        }
    });
   }


  var text = document.createTextNode("");
  var stepsNr = 0;
  var toggle = true;
  var oldData = null;

  $(function(){
    $(".nav-sidebar a").click(function(){
        $(this).tab('show');
        $('#inhabRequest').collapse('show');
        });

        $('.nav-sidebar a[href="#steps"]').on('shown.bs.tab', function(){
            mkSteps(stepsNr);
            $('#inhabRequest').collapse('show');
            $.get("showUninhabitedTy", function(data){
                 $('#uninhabitedTy, #unusableComb').click(function(e){
                      if(this.id == 'uninhabitedTy'){
                         $('#collapse1').collapse('toggle');
                         $("#debuggerMessages").html(data.replace(/\n/g, '<br />'));
                      }
                 });
            });
            $.get("showUnusableCMsg", function(data){
                   $('#uninhabitedTy, #unusableComb').click(function(e){
                      if(this.id == 'unusableComb'){
                         $('#collapse2').collapse('toggle');
                         $("#debuggerM").html(data.replace(/\n/g, '<br />'));
                      }
                   });
            });
            });

         $('#message-text').keypress(function(evt){
        		var charCode = (evt.which) ? evt.which : evt.keyCode;
        		if (charCode != 46 && charCode > 31 && (charCode < 48 || charCode > 57))
        			return false;
        			return true;
        });


        $('.nav-sidebar a[href="#results"]').on('shown.bs.tab', function(){
                 $('#inhabRequest').collapse('show');

                 var elements = document.getElementsByClassName("navbar navbar-default");
                    if(elements.length != 0){
                        var length = elements.length;
                        for (var i=0; i<= length-1; i++) {
                            elements[0].remove();
                        }
                    }
                 $.get("countSolutions", function(result){
                 if (!parseInt(result)){
                 $('#isInfinite').modal('show');
                 $('#exampleModalLongTitle').html(result);
                 $('#save').click(function(){
                     var number = document.getElementById("message-text").value;
                     $('#isInfinite').modal('hide');
                     makeSolutions(number);
                   });
                }else {
                    makeSolutions(result);
                }



               });

            });
        function makeSolutions(number){
            for (var item = 0; item < number ; item ++) {
                var nav = document.createElement("nav");
                nav.className = "navbar navbar-default";
                nav.id = "nav";

                var div = document.createElement("div");
                div.className = "container-fluid";

                var divNavbar = document.createElement("navBar");
                divNavbar.className = "navbar-header";
                divNavbar.id = "divNav" + item;

                //create variation buttons

                if(!document.getElementById(item)){
                   var divBtn = document.createElement("divBtn");
                   var txt = document.createTextNode("Variation "+ item + ":");
                   var btn = document.createElement("button");
                   btn.className = "btn btn-primary";
                   btn.id = item;
                   div.appendChild(divNavbar);
                   nav.appendChild(div);
                   divBtn.appendChild(btn);
                   btn.appendChild(txt);
                   divNavbar.appendChild(divBtn);
                   document.getElementById("results").appendChild(nav);

                   //on click shows one solution
                   btn.addEventListener("click", function(e){
                      $('#inhabRequest').collapse('hide');
                      var allElements = document.querySelectorAll('*[id]')
                      var allIds = [];
                      for (var i = 0, n = allElements.length; i < n; ++i) {
                         var el = allElements[i];
                         if (el.id != e.target.id) {
                             $('#'+el.id).prop('disabled', false);
                         }else{
                             $('#'+e.target.id).prop('disabled', true);
                         }
                      }
                      $("divSol").remove();
                      var divSol = document.createElement("divSol");
                      divSol.className = "container-fluid";
                      $('.divSol').addClass('collapse');
                      divSol.id =  "solutionSol" + e.target.id;
                      var solution = document.createElement("solution");
                      $.get("showResult/" +  parseInt(e.target.id), function(data){
                          showPartGraph(parseInt(e.target.id));
                          text = document.createTextNode(data);

                          divSol.appendChild(solution);
                          solution.appendChild(text);
                      });
                                                         //You can show the used combinators in the graph
                                                         //$.get("showUsedCombinators/" +  parseInt(e.target.id), function(data){
                                                           //showPartGraph(parseInt(e.target.id));
                                                           /*var text1 = document.createTextNode(data);

                                                         divSol.appendChild(solution);
                                                         solution.appendChild(text1);
                                                         });
                                                         divSol.appendChild(solution);
                                                                                      solution.appendChild(text);*/
                      document.getElementById("divNav"+parseInt(e.target.id)).appendChild(divSol);
                   });
                }
            }
        }

        $('.nav-sidebar a[href="#repo"]').on('shown.bs.tab', function(){

            $('#inhabRequest').collapse('show');
            $.get("repository", function(data){
              $("#repository").html(data.replace(/\n/g, '<br />'));
               });
        });

        /*$('.nav-sidebar a[href="#paths"]').on('shown.bs.tab', function(){
            if( $('#inhabRequest').is( ":visible" )){
            console.log("true", $('#inhabRequest').is( ":visible" ));
                $('#inhabRequest').removeClass('in');
            }
                    var combName = "down";
                    $.get("showPaths/"+combName, function(data){
                      $("#combinatorTys").html(data.replace(/\n/g, '<br />'));
                       });

                    $.get("getCombinators", function(data){
                      $("#combinatorName").html(data.replace(/\n/g, '<br />'));
                                           });
                });*/
           /*function showPaths(label) {

           $('.nav-sidebar a[href="#paths"]').tab('show');
          // $('.nav-sidebar a[href="#paths"]').on('shown.bs.tab', function(){
                       if( $('#inhabRequest').is( ":visible" )){
                       console.log("true", $('#inhabRequest').is( ":visible" ));
                           $('#inhabRequest').removeClass('in');
                       }
                               $.get("showPaths/"+label, function(data){
                                 $("#combinatorTys").html(data.replace(/\n/g, '<br />'));
                                  });

                               $.get("getCombinators", function(data){
                                 $("#combinatorName").html(data.replace(/\n/g, '<br />'));
                                                      });
                          // });
           }*/

        $('.nav-sidebar a[href="#mess"]').on('shown.bs.tab', function(){
            $.get("showDebuggerMessages", function(data){
            $('#inhabRequest').collapse('show');
             $("#showDebuggerMessages").html(data);
            });
        });

   $('#forwardButton, #backwardButton, #toggleCyclesButton').click(function(){
        if(this.id == 'toggleCyclesButton'){
            if (toggle == true){
              toggleCycle(stepsNr);
              toggle = false;
            }else{
              mkSteps(stepsNr);
              toggle = true;
             }
        }
        if(this.id == 'forwardButton'){
            $.get("steps/" + stepsNr, function(data){
              if (data != "No more steps!") {
                stepsNr ++;
                toggle = true;
                mkSteps(stepsNr);
              }else {
                $("#cy-steps").html("<h3>" + data.replace(/\n/g, '<br />') + "</h3>");
              }
            });
        }
        else{
            if(this.id == 'backwardButton'){
            stepsNr --;
            toggle = true;
            mkSteps(stepsNr);
           }
        }
    });


   function showPartGraph(index){
      $.get("showOnePossibleSolutionGraph/" + index, function(data){
         try {
             var graph = JSON.parse(data);
             mkGraph(graph, "#cy-part-graph");
         }
         catch{
             $("#cy--part-graph").html(data.replace(/\n/g, '<br />'));
         }
       });
   }

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

    function showPaths(label) {
           $('.nav-sidebar a[href="#paths"]').tab('show');
           $("#combinatorName").html(label);

            if( $('#inhabRequest').is( ":visible" )){
                           $('#inhabRequest').removeClass('in');
                       }
                       var number = 1;
            $.get("showPaths/"+label, function(data){
                 $("#combinatorTys").html(data.replace(/\n/g, '<br />'));
             });
           }

        $(document).on("change", ".form-check-input", function(e){
              var path = null;
              if($(this).is(':checked')) {
                path = ($(this).val()).toString();
                $.get("showToCover/" + path, function(data){
                    var path1 = path.replace('*', '&#42;')
                    $("#targetsToCover").prepend('<li id = "'+ path1 + '">' + data.replace(/\n/g, ' ') + '</li>');
                });



              } else {
                  var id = ($(this).val()).toString();
                  var elem = document.getElementById(id);
                  elem.remove();

                                                  }
                                                      });

    function computeNewRequest(request) {
        if (request.includes("[")){
        }else {
    } var req = request.replace(/\[/g, '91')
    req = req.replace(/\]/g, '93')
        $.get("computeRequest/" + req, function(data){
            $.get("graph", function(data){
                     var graph = JSON.parse(data)
                     $("#cy-graph").html(" ");
                     mkGraph(graph, "#cy-graph");
            });
        });
    }


      function mkSteps(stepsNr) {
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
                         selector: 'node[style = "invisible-unusable-combinator-node"]',
                         css: {
                           'visibility': 'hidden',
                           'text-valign': 'top',
                           'text-halign': 'center',
                           'padding-top': '3em',
                           'padding-left': '10px',
                           'padding-bottom': '10px',
                           'padding-right': '10px',
                           'background-color': '#FF3100',
                           'shape' : 'ellipse',
                           'opacity' : '0'
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
                 var tappedBefore;
                 var tappedTimeout;
                 cy.on('tap', function(event) {
                   var tappedNow = event.target;
                   if (tappedTimeout && tappedBefore) {
                     clearTimeout(tappedTimeout);
                   }
                   if(tappedBefore === tappedNow) {
                     tappedNow.emit('doubleTap');
                     tappedBefore = null;
                   } else {
                     tappedTimeout = setTimeout(function(){ tappedBefore = null; }, 300);
                     tappedBefore = tappedNow;
                   }
                 });

                 cy.filter('node[style = "combinator-node"], node[style = "unusable-combinator-node"]').on('doubleTap', function (evt) {
                                 var combinatorName = evt.target._private.data.label;
                                  showPaths(combinatorName);

                                });

    }

});
