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

require(['bootstrap', 'cytoscape'], function (bootstrap, cytoscape) {
    loadDoc();
    $(function () {
        // loadDoc();
        // Ajax to data

        $.get("graph", function (data) {
            $("#progress").html(" ");
            try {
                var graph = JSON.parse(data);
                mkGraph(graph, "#cy-graph");
            } catch {
                var news = loadDoc();
                //  $("#cy-graph").html(data.replace(/\n/g, '<br />'));
                $("#cy-graph").html(data);
            }
        });

       /* $.get("appGraph", function (data) {
            //$("#cy-app-graph").html(" ");
            try {
                var graph = JSON.parse(data);
                mkGraph(graph, "#cy-app-graph");
                console.log("hallo  try");
            } catch {
                console.log("hallo  catch");
                var news = loadDoc();
                $("#cy-app-graph").html(data.replace(/\n/g, '<br />'));
            }
            //   $("#cy-app-graph").html(data.replace(/\n/g, '<br />'));
        });*/

        $(".nav-tabs a").click(function () {

            if ($(this).text() == "Applicative Tree Grammar") {
                $.get("appGraph", function (data) {
                    //$("#cy-app-graph").html(" ");
                    try {
                        var graph = JSON.parse(data);
                        mkGraph(graph, "#cy-app-graph");
                    } catch {
                        var news = loadDoc();
                        $("#cy-app-graph").html(data.replace(/\n/g, '<br />'));
                    }
                    //   $("#cy-app-graph").html(data.replace(/\n/g, '<br />'));
                });
            }
        });
    });

    function loadDoc() {
        $.ajax({
            url: "ide",
            async: true,
            xhr: function () {
                var xhr = new window.XMLHttpRequest();
                //Upload Progress
                xhr.addEventListener("progress", function (evt) {
                    if (evt.lengthComputable) {
                        var percentComplete = (evt.loaded / evt.total) * 100;
                        $("#dynamic").css({"width": percentComplete + "%"});
                        percentComplete;
                    }
                }, false);
                return xhr;
            }
        });
    }


    var text = document.createTextNode("");
    var stepsNr = 0;
    var toggle = true;
    var oldData = null;
    var warn = $('#warnings').collapse('hide');
    var warData = "";

    $(function () {
        $(".nav-sidebar a").click(function () {
            $(this).tab('show');
            // $('#inhabRequest').collapse('hide');
        });


        $('.nav-sidebar a[href="#graph"]').on('shown.bs.tab', function () {

            $('#inhabRequest').collapse('show');
        });

        $('.nav-sidebar a[href="#steps"]').on('shown.bs.tab', function () {
            mkSteps(stepsNr);
            $('#inhabRequest').collapse('hide');

            $('#uninhabitedTy, #unusableComb, #warnings').click(function (e) {

                if (this.id == 'uninhabitedTy') {
                    $.get("showUninhabitedTy", function (data) {
                        $('#collapse1').collapse('toggle');
                        $("#debuggerMessages").html(data.replace(/\n/g, '<br />'));
                    });
                } else {
                    if (this.id == 'unusableComb') {
                        $.get("showUnusableCMsg", function (data) {
                            $('#collapse2').collapse('toggle');
                            $("#debuggerM").html(data.replace(/\n/g, '<br />'));

                        });
                    }
                    if (this.id == 'warnings') {
                        $('#collapse3').collapse('toggle');
                        $("#warningMessages").html(warData.replace(/\n/g, '<br />'));

                    }
                }
            });
        });

        $('#message-text').keypress(function (evt) {
            var charCode = (evt.which) ? evt.which : evt.keyCode;
            if (charCode != 46 && charCode > 31 && (charCode < 48 || charCode > 57))
                return false;
            return true;
        });


        $('.nav-sidebar a[href="#results"]').on('shown.bs.tab', function () {
            $('#inhabRequest').collapse('hide');
            var elements = document.getElementsByClassName("navbar navbar-default");
            if (elements.length != 0) {
                var length = elements.length;
                for (var i = 0; i <= length - 1; i++) {
                    elements[0].remove();
                }
            }
            $.get("countSolutions", function (result) {

                if (result == 0) {
                    $('#cy-part-graph').html("Inhabitant not found!");
                } else {
                    if (!parseInt(result)) {
                        $('#isInfinite').modal('show');
                        $('#exampleModalLongTitle').html(result);
                        $('#save').click(function () {
                            var number = document.getElementById("message-text").value;
                            $('#isInfinite').modal('hide');
                            //TODO remove x2, x3, x4....
                            //console.log(number);
                            makeSolutions(number, "Variation", "results");
                            makeSolutions(number, "Download", "download");
                        });
                        $('#isInfinite').on('hidden', function(){
                            $(this).data('modal', null);
                        });
                    } else {
                        makeSolutions(result, "Variation", "results");
                        makeSolutions(result, "Download", "download");
                    }
                }


            });

        });
        $('.nav-sidebar a[href="#taxonomy"]').on('shown.bs.tab', function () {
            $('#inhabRequest').collapse('hide');
            var elements = document.getElementsByClassName("navbar navbar-default");
            if (elements.length != 0) {
                var length = elements.length;
                for (var i = 0; i <= length - 1; i++) {
                    elements[0].remove();
                }
            }
            $.get("showTaxonomy", function (result) {
                if (result == 0) {
                    $('#cy-taxonomy-graph').html("No taxonomy defined!");
                } else {
                    $.get("getTaxonomySize", function (number) {
                        makeSolutions(number, "Taxonomy " + result, "taxonomy");
                    });
                }
            });

        });

        function makeSolutions(number, nameButtonPart, direction) {
            var nameButton = ""

            for (var item = 0; item < number; item++) {
                if(direction != "download") {
                    var nav = document.createElement("nav");
                    nav.className = "navbar navbar-default well";
                    nav.id = "nav";
                    var div = document.createElement("div");
                    div.className = "container-fluid download";
                    var divNavbar = document.createElement("navBar");
                    divNavbar.className = "navbar-header";
                    divNavbar.id = "divNav" + item;
                }

                //create variation or taxonomy buttons
                if (!document.getElementById(item)) {
                    if (direction == "taxonomy") {
                        nameButton = nameButtonPart
                    } else {
                        nameButton = nameButtonPart + " " + item
                    }
                    var divBtn = document.createElement("divBtn");
                    var txt = document.createTextNode(nameButton + ":");
                    var btn = document.createElement("button");
                    if(direction == "download") {
                        btn.className = "btn btn-primary btn-download";
                    }else{
                        btn.className = "btn btn-primary";
                    }
                    btn.id = item + direction+nameButtonPart;
                    if(direction != "download") {
                        div.appendChild(divNavbar);
                        nav.appendChild(div);
                        divNavbar.appendChild(divBtn);
                        document.getElementById(direction).appendChild(nav);
                    }else{
                        var downloadNavbar = document.getElementById("divNav" + item);
                        downloadNavbar.appendChild(divBtn);
                    }
                    divBtn.appendChild(btn);
                    btn.appendChild(txt);

                    //on click shows one solution
                    btn.addEventListener("click", function (e) {
                        $('#inhabRequest').collapse('hide');
                        var allElements = document.querySelectorAll('*[id]')
                        var allIds = [];
                        for (var i = 0, n = allElements.length; i < n; ++i) {
                            var el = allElements[i];
                            if (el.id != e.target.id) {
                                $('#' + el.id).prop('disabled', false);
                            } else {
                                $('#' + e.target.id).prop('disabled', true);
                            }
                        }
                        $("divSol").remove();
                        var divSol = document.createElement("divSol");
                        divSol.className = "container-fluid";
                        $('.divSol').addClass('collapse');
                        divSol.id = "solutionSol" + e.target.id;
                        var solution = document.createElement("solution");
                        if (direction == "taxonomy") {
                            showTaxonomyGraph();
                        } else {
                            if(direction == "download"){
                                    // Generate download of hello.txt file with some content
                                //var filename = e.target.id;

                                $.get("getSolutionSize/" + parseInt(e.target.id), function (size) {
                                    if(size!= 0){
                                        for(i = 1; i <=size; i ++){
                                            const filename = triString(parseInt(e.target.id))+"_"+i.toString() + ".p";
                                        $.get("showInterpretation/" + parseInt(e.target.id)+"/"+i, function (text) {
                                            download(filename, text, false);
                                        });}
                                    }
                                    $.get("getXMLInterpretation/" + parseInt(e.target.id), function (xml) {
                                        var filename = "jts_out_inhabitant_"+triString(parseInt(e.target.id))+".xml";
                                        download(filename, xml, true);
                                    });
                                });

                            }else{
                            $.get("showResult/" + parseInt(e.target.id), function (data) {
                                showPartGraph(parseInt(e.target.id));
                                text = document.createTextNode(data);
                                divSol.appendChild(solution);
                                solution.appendChild(text);
                            });
                        }
                        }
                        document.getElementById("divNav" + parseInt(e.target.id)).appendChild(divSol);
                    });
                }
            }
        }

        function triString(num){
            const size = 3
                var s = num+"";
                while (s.length < size) s = "0" + s;
            return s;
        }
        function download(filename, text, xml) {
           var element = document.createElement('a');
            var parsedText
            if(xml){
                parsedText = (new XMLSerializer()).serializeToString(text)
            }else{
                parsedText = encodeURIComponent(text);
            }
            element.setAttribute('href', 'data:text/plain;charset=utf-8,' + parsedText);
            element.setAttribute('download', filename);
            element.style.display = 'none';
            document.body.appendChild(element);
            element.click();
            document.body.removeChild(element);
        }


        $('.nav-sidebar a[href="#repo"]').on('shown.bs.tab', function () {

            $('#inhabRequest').collapse('hide');
            $.get("repository", function (data) {
                $("#repository").html(data.replace(/\n/g, '<br />'));
            });
            $.get("repositoryWithoutVars", function (data) {
                $("#repositoryWithoutVars").html(data.replace(/\n/g, '<br />'));
            });
        });

        $('.nav-sidebar a[href="#filter"]').on('shown.bs.tab', function () {
            $('#inhabRequest').collapse('hide');
            $("#submitFilter").click(function () {
                var request = document.getElementById("filterEntry").value;
                var req = request.replace(/\[/g, '91')
                req = req.replace(/\]/g, '93')
                $.get("filter/" + req, function (data) {
                    $("#cy-filter-graph").html(" ");
                    try {
                        var graph = JSON.parse(data);
                        mkGraph(graph, "#cy-filter-graph");
                    } catch {
                        $("#cy-filter-graph").html(data.replace(/\n/g, '<br />'));
                    }
                    $.get("filterRequest/" + req, function (e) {
                        $("#newFilterRequest").html("<label for=&quot;request&quot;><h4 class=&quot;secondary&quot;>&Gamma; &vdash; ? : " + e + "</h4></label>");
                    });
                });
            });
        });

        $('.nav-sidebar a[href="#smt"]').on('shown.bs.tab', function () {
            $('#inhabRequest').collapse('hide');
            $.get("smt", function (data) {
                if (data.includes("not")) {
                    $("#grammarToModel").html(data.replace(/\n/g, '<br />'));
                    $("#submitCheckbox").prop("disabled", true);
                } else {
                    $("#grammarToModel").html(data.replace(/\n/g, '<br />'));
                }
            });
        });


        $('.nav-sidebar a[href="#mess"]').on('shown.bs.tab', function () {
            $('#inhabRequest').collapse('hide');
            $('#showWarningsPanel').collapse('hide');
            $.get("showUnusedCombinators", function (data) {
                $("#showUnusedCombinators").html(data);
            });
            $.get("showUninhabitedTypes", function (data) {
                $("#showUninhabitedTypes").html(data);
            });
            if (warData != "") {
                $('#showWarningsPanel').collapse('show');
                $("#showWarnings").html(warData);
            }
        });

        $('.nav-sidebar a[href="#paths"]').on('shown.bs.tab', function () {
            showPaths(null);
            $(document).on("change", "[name='optradioCovering']", function (e) {
                var radioVal = $(this).val();
                showPaths(radioVal);
            });
            $(document).on("change", "[name='optradio']", function (e) {
                var radioVal = $(this).val();
                $.get("showPaths/" + radioVal, function (data) {
                    $("#targetsToCover").html("");
                    $("#combinatorTys").html(data.replace(/\n/g, "<br />"));
                });

            });
            $("#resetCover").click(function () {
                showPaths(null);
            });

        });

        $('#forwardButton, #backwardButton, #toggleCyclesButton').click(function () {
            if (this.id == 'toggleCyclesButton') {
                if (toggle == true) {
                    toggleCycle(stepsNr);
                    toggle = false;
                } else {
                    mkSteps(stepsNr);
                    toggle = true;
                }
            }
            if (this.id == 'forwardButton') {
                stepsNr++;
                $.get("steps/" + stepsNr, function (data) {
                    if (data != "No more steps!") {
                        toggle = true;
                        mkSteps(stepsNr);
                        $.get("showWarnings", function (data) {
                            if (data != "No warnings!") {
                                warn = $('#warnings').collapse('show');
                                warData = data;
                            }
                        });
                    } else {
                        $("#cy-steps").html("<h3>" + data.replace(/\n/g, '<br />') + "</h3>");
                    }
                });

            } else {
                if (this.id == 'backwardButton') {
                    stepsNr--;
                    toggle = true;
                    mkSteps(stepsNr);
                }
            }
        });


        function showPartGraph(index) {
            $.get("showOnePossibleSolutionGraph/" + index, function (data) {
                try {
                    var graph = JSON.parse(data);
                    mkGraph(graph, "#cy-part-graph");
                } catch {
                    $("#cy-part-graph").html(data.replace(/\n/g, '<br />'));
                }
            });
        }

        function showTaxonomyGraph() {
            $.get("showTaxonomyGraph/", function (data) {
                try {
                    var graph = JSON.parse(data);
                    mkGraph(graph, "#cy-taxonomy-graph");
                } catch {
                    $("#cy--taxonomy-graph").html(data.replace(/\n/g, '<br />'));
                }
            });
        }

        function toggleCycle(stepNr) {
            $.getJSON("toggleCycle/" + stepsNr, function (data) {
                mkGraph(data, "#cy-steps")
            });
        }
    });


    $("#submit").click(function () {
        var x = document.getElementById("request").value;
        computeNewRequest(x);
    });

    $("#submitCheckbox").click(function () {
        var load = $(this);
        var checkBox = document.getElementsByName('optCheckSMT');
        var txt = "";
        var i;
        for (i = 0; i < checkBox.length; i++) {
            if (checkBox[i].checked) {
                txt = txt + "tag=" + checkBox[i].value + "&";
            }
        }
        if (txt != "") {
            load.prop("disabled", true);
            load.html(
                '<span class="fa fa-spinner fa-spin spinner-border spinner-border-sm" role="status" aria-hidden="true"></span> Loading...');
            txt = txt.slice(0, -1);
            $.get("inhabitantsWithoutCombinator/items" + "?" + txt, function (data) {
                if (document.getElementById("resultsTree") == null) {
                    var pre = document.createElement("pre");
                    pre.className = "well";
                    pre.id = "resultsTree";
                    var h3 = document.createElement("h3");
                    var txt = document.createTextNode("Result: ");
                    h3.appendChild(txt);
                    pre.appendChild(h3);
                    var div = document.createElement("div");
                    div.id = "treeResult";
                    pre.appendChild(div);
                    document.getElementById("smt").appendChild(pre);
                }
                $("#treeResult").html(data.replace(/\n/g, '<br />'));
                load.prop("disabled", false);
                load.html('Filter');
            });
        } else {
            alert("Choose Filter!");
        }
    });

    function showPaths(label) {
        if (label != null) {
            $('.nav-sidebar a[href="#paths"]').tab('show');
            $("#combinatorName").html(label);
            /* if( $('#inhabRequest').is( ":visible" )){
                            $('#inhabRequest').removeClass('in');
                        }*/
            $.get("computeNumberOfArgs/" + label, function (data) {
                $("#combinatorTys").html("");
                $("#targetsToCover").html("");
                $("#numberOfArgs").html(data.replace(/\n/g, '<br />'));

            });
        } else {
            $.get("repositoryCovering", function (data) {
                $("#combinatorName").html(data.replace(/\n/g, '<br />'));
            });
        }
    }

    $(document).on("change", "[name='checkToCover']", function (e) {
        var path = null;
        if ($(this).is(':checked')) {
            path = ($(this).val()).toString();
            $.get("showToCover/" + path, function (data) {
                var path1 = path.replace('*', '&#42;')
                $("#targetsToCover").html(data.replace(/\n/g, '<br />'));

            });


        } else {
            // $("#targetsToCover").html("");
            var id = ($(this).val());
            var elem = document.getElementById("targetsToCover");
            var elem_child = document.getElementsByName(id.toString());

            $("li[name='" + id + "']").remove();
            // $('li [name='id']').remove();
            /* elem_child.parentNode.removeChile(elem_child);
             elem.appendChild(elem_child);*/

        }
    });

    function computeNewRequest(request) {
        var req = request.replace(/\[/g, '91')
        req = req.replace(/\]/g, '93')
        $.get("computeRequest/" + req, function (data) {
            $("#cy-graph").html(" ");
            try {
                var graph = JSON.parse(data);
                mkGraph(graph, "#cy-graph");
            } catch {
                $("#cy-graph").html(data.replace(/\n/g, '<br />'));
            }
        });
    }


    function mkSteps(stepsNr) {
        $.getJSON("steps/" + stepsNr, function (data) {
            mkGraph(data, "#cy-steps");
        });
    }


    function mkGraph(graph, canvas) {
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
                        'shape': 'roundrectangle'

                    }
                },
                {
                    selector: 'node[style = "apply-node"]',
                    css: {
                        'padding-top': '10px',
                        'padding-left': '10px',
                        'padding-bottom': '10px',
                        'padding-right': '10px',
                        'text-valign': 'top',
                        'text-halign': 'center',
                        'background-color': '#70ABFF',
                        'shape': 'roundrectangle'

                    }
                },
                {
                    selector: 'node[style = "subType-node"]',
                    css: {
                        'padding-top': '10px',
                        'padding-left': '10px',
                        'padding-bottom': '10px',
                        'padding-right': '10px',
                        'text-valign': 'top',
                        'text-halign': 'center',
                        'background-color': '#428CCA',
                        'shape': 'roundrectangle'

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
                        'shape': 'roundrectangle'
                    }
                },

                {
                    selector: 'node[style = "uninhabited-type-node"]',
                    css: {
                        //     'visibility': 'hidden',
                        'padding-top': '10px',
                        'padding-left': '10px',
                        'padding-bottom': '10px',
                        'padding-right': '10px',
                        'text-valign': 'top',
                        'text-halign': 'center',
                        'background-color': '#FFE69A',
                        'border-style': 'solid',
                        'border-opacity': '0.7',
                        'border-width': '10px',
                        'border-color': '#FF3100',
                        'shape': 'roundrectangle'
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
                        'shape': 'ellipse'
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
                        'shape': 'ellipse'
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
                        'shape': 'ellipse',
                        'opacity': '0'
                    }
                },
                {
                    selector: 'node[style = "argument-node"]',
                    css: {
                        'text-valign': 'center',
                        'text-halign': 'center',
                        'background-color': '#FF9400',
                        'shape': 'octagon'
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
        cy.resize();
        //cy.invalidateDimensions()

        var layoutName = 'breadthfirst';

        // Callback for relayout
        var adjustLayout = function () {
            var cyDiv = $(canvas);
           /* if(canvas== "#cy-app-graph"){
                //cy.invalidateDimensions()
                cyDiv.offset({
                    top: 416.3333435058594,
                    left: 215})
            }*/
            var main = $(window);
            var delta = cyDiv.outerHeight(true) - cyDiv.innerHeight();
            var parents = cyDiv.parentsUntil(main);
            $.each(parents, function (pos, value) {
                delta = delta + $(value).outerHeight(true) - $(value).innerHeight();
            });

            cyDiv.height(main.height() - cyDiv.offset().top - delta - 20);

            cy.resize();
            if (layoutName != 'custom') {
                cy.filter('node[style != "type-node"]').layout({
                    name: 'cose',
                    condense: true,
                    fit: false,
                    padding: 75,
                    avoidOverlapPadding: 75,
                    ready: function () {
                    }
                }).run();

                cy.filter('node[style != "subType-node"]').layout({
                    name: 'cose',
                    condense: true,
                    fit: false,
                    padding: 75,
                    avoidOverlapPadding: 75,
                    ready: function () {
                    }
                }).run();
                /*
                                     cy.layout({
                                     name: 'breadthfirst',
                                     condense: true,
                                     fit: false,
                                     padding: 75,
                                     roots: 'node[style = "combinator-node"]',
                                     avoidOverlapPadding: 75,
                                     ready: function() { }
                                                          }).run();*/
            }

            cy.layout({
                name: layoutName,
                boundingBox: {x1: 0, y1: 0, w: cyDiv.width(), h: cyDiv.height()},
                padding: 30,
                animate: true,
                avoidOverlap: true,
                avoidOverlapPadding: 75,
                ready: function () {
                }
            }).run()

        }

        adjustLayout();

        // Adjust layout on window size change
        window.resizeEvt;
        $(window).resize(function () {
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
        cy.nodes().forEach(function (node) {
            node.on("free", function (e) {
                layoutName = 'Custom';
                $('#layout-dropdown').find('button').text(layoutName).append(' <span class="caret"></span>');
            });

        });
        var tappedBefore;
        var tappedTimeout;
        cy.on('tap', function (event) {
            var tappedNow = event.target;
            if (tappedTimeout && tappedBefore) {
                clearTimeout(tappedTimeout);
            }
            if (tappedBefore === tappedNow) {
                tappedNow.emit('doubleTap');
                tappedBefore = null;
            } else {
                tappedTimeout = setTimeout(function () {
                    tappedBefore = null;
                }, 300);
                tappedBefore = tappedNow;
            }
        });

        cy.filter('node[style = "combinator-node"], node[style = "unusable-combinator-node"]').on('doubleTap', function (evt) {
            var combinatorName = evt.target._private.data.label;
            showPaths(combinatorName);

        });

    }

});
