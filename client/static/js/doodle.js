/* ------------------------------------------------------------------------
    Title:          Tiny Doodle
    
    Version:        0.2
    URL:            http://tinydoodle.com/
    
    Description:
        Tiny Doodle is an exercise in learning about <canvas>.
        Event handlers are attached the to <canvas> elemet for both
        mouse and touch input devices. The user can doodle away on the
        <canvas>, clear and save the resulting doodle.
        
    Author:         Andrew Mason
    Contact:        a.w.mason at gmail dot com
    Author's Site:  http://analoguesignal.com/
    
    Requirements:
        * Jquery 1.3+
    
    Changelog:
        0.1 (28th May 2009)
            - First demo build
        0.2 (30th May 2009)
            - Addded Pen and Eraser
            - Commented code
            - 
    
    Todo:
        * Error checking and handling
        * Clean up code
        * Add yellow throber to indicate added images
        * Add share links
    
    Licence:
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

------------------------------------------------------------------------ */

// Run once the DOM is ready
$(document).ready(function () {

    var savedServer =  window.localStorage.getItem('serverAddr');
    var serverURL = 'ws://localhost:12345';
    var enableServerChanges = false;
    if (enableServerChanges && savedServer != null) {
      serverURL = savedServer;
      console.log("saved server: "+serverURL);
      $('#serverBox').val(serverURL);
    }

    console.log("Attempting to connect to " + serverURL);
    var socket = new WebSocket(serverURL);

    doodle.init(socket);

    socket.onopen = function(event) {
      socket.send('HELLO');
    };

    socket.onmessage = function(event) { 
    if ((/^DRAW /).test(event.data )) {
      parts = event.data.split(" ");
      var x0 = parseInt(parts[1]);
      var y0 = parseInt(parts[2]);
      var x1 = parseInt(parts[3]);
      var y1 = parseInt(parts[4]);
      doodle.setRemotePen();
      doodle.drawfromto(x0, y0, x1, y1);
    } else if ((/\{\"mteams\"/).test(event.data)) {
      console.log("updating teams with: " + event.data);
      updateTeams(JSON.parse(event.data));//TODO: security.  only parse what you trust 100%
      parts = event.data.split(" ");
      var x0 = parseInt(parts[2]);
    } else if ((/^GUESS /).test(event.data )) {
      parts = event.data.split(" ");
      var rightness = parts[1];
      var who = parts[2];
      var guess = parts[3];
      console.log("got guess: " + guess + " by " + who + " right? "+rightness );
      $(doodle.noticeID).prepend("<li>"+who + " guessed " + guess + " " +rightness+ "</li>");
    } else if ((/^STATE /).test(event.data )) {
      parts = event.data.split(" ");
      var st = parts[1];
      $('#playstate').html(st);
    } else if ("ROUNDSTART" == event.data ) {
      console.log("Round starts!");
      doodle.newDoodle();
    } else if ((/^ROLE /).test(event.data) ) {
      console.log("Role received: " + event.data);
      parts = event.data.split(" ");
      var role = parts[1];
      if (role=="artist") {
        var word = parts[3];
        $('#wordtodraw').html(word);
      } else {
        $('#wordtodraw').html(" -- this round, you must guess!");
      }
    } else if ("ok" == event.data ) {
    } else {
      console.log("GOT unrecognised ws msg: " + event.data);
      $(doodle.noticeID).prepend("<li>GOT: " + event.data + "</li>");
    }};

    socket.onclose = function(event) { 
      $('#playstate').html("<span style='color: red'>DISCONNECTED</span>");
      console.log('The websocket to the game server is closed.');
    }

    socket.onerror = function(event) { alert('An error occurred with the game websocket: ' + event + " Data: " + event.data); }

});

updateTeams = function(teamsData) {
  $('#team0list').empty();
  $('#team1list').empty();
  for (var tix = 0; tix < teamsData.mteams.length; tix++) {
    var t = teamsData.mteams[tix];
    var sel = '#team'+t.teamId+'score';
    $(sel).html(""+t.mscore);
    for (var pix = 0; pix < t.members.length; pix++) {
      var p = t.members[pix];
      $('#team'+t.teamId+'list').append("<li>"+p+"</li>"); //TODO: security
    }
  }
}

var doodle = {
    // Define some variables
    'drawing':          false,
    'linethickness':    2,
    'updating':         false,
    'saveID':           '#save',
    'newID':            '#new',
    'penID':            '#pen',
    'voteSkipID':       '#voteskip',
    'guessBoxID':       '#guessBox',
    'nameBoxID':        '#nameBox',
    'serverBoxID':      '#serverBox',
    'saveServerID':     '#saveServer',
    'noticeID':         '#notification',
    'loaded_id':         false,
    'lineSegs':          []
};

doodle.init = function(givenSocket) {
    doodle.socket = givenSocket;
    // Collect elements from the DOM and set-up the canvas
    doodle.canvas = $('#doodle_canvas')[0];
    doodle.context = doodle.canvas.getContext('2d');
    
    doodle.newDoodle();
    
    // Mouse based interface
    $(doodle.canvas).bind('mousedown', doodle.drawStart);
    $(doodle.canvas).bind('mousemove', doodle.draw);
    $(doodle.canvas).bind('mouseup', doodle.drawEnd);
    $('body').bind('mouseup', doodle.drawEnd);
    
    // Touch screen based interface
    $(doodle.canvas).bind('touchstart', doodle.drawStart);
    $(doodle.canvas).bind('touchmove', doodle.draw);
    $(doodle.canvas).bind('touchend', doodle.drawEnd);
    
    // Add save event to save button
    $(doodle.saveID).bind('click', doodle.saveImage);
    
    // Add clear canvas event
    $(doodle.newID).bind('click', doodle.newDoodle);
    
    // Add Pen selection event
    $(doodle.penID).bind('click', doodle.pen);
    $(doodle.voteSkipID).bind('click', doodle.voteSkip);
    $(doodle.guessBoxID).bind('keyup', doodle.guessBoxKeyup);
    $(doodle.nameBoxID).bind('keyup', doodle.nameBoxKeyup);
    $(doodle.saveServerID).bind('click', doodle.saveServer);
};


doodle.newDoodle = function(src, id) {
    doodle.clearCanvas();
    if (!src) {
        src = 'static/images/blank.gif';
    }
    
    if (!id) {
        id = '';
    }
}

doodle.clearCanvas = function(ev) {
    // Clear existing drawing
    doodle.context.clearRect(0,0, doodle.canvas.width, doodle.canvas.height);
    doodle.canvas.width = doodle.canvas.width;
    
    // Set the background to white.
    // then reset the fill style back to black
  
    doodle.context.fillStyle = '#000000';
    doodle.context.fillRect(0, 0, doodle.canvas.width, doodle.canvas.height);
    doodle.context.fillStyle = '#D9D4B7';
    doodle.context.fillRect(1, 1, doodle.canvas.width-2, doodle.canvas.height-2);
    doodle.context.fillStyle = '#000000';
    
    // Remove active class from other thumbs
    $('#output IMG').each(function() {
        $(this).removeClass('active');
    });
    
    // Set the drawning method to pen
    doodle.pen();
    
    // Flag that the user is working on a new doodle
    doodle.updating = false;
}

doodle.sendLineSeg = function(x0, y0, x1, y1) {
  doodle.socket.send("DRAW: "+x0 + " "+y0+" "+x1+" "+y1);
}

doodle.drawStart = function(ev) {
    ev.preventDefault();
    // Calculate the current mouse X, Y coordinates with canvas offset
    var x, y;
    x = ev.pageX - $(doodle.canvas).offset().left;
    y = ev.pageY - $(doodle.canvas).offset().top;
    doodle.drawing = true;
    doodle.context.lineWidth = doodle.linethickness;

    // Store the current x, y positions
    doodle.oldX = x;
    doodle.oldY = y;
}

doodle.draw = function(ev) {
    // Calculate the current mouse X, Y coordinates with canvas offset
    var x, y;
    x = ev.pageX - $(doodle.canvas).offset().left;
    y = ev.pageY - $(doodle.canvas).offset().top;
    
    // If the mouse is down (drawning) then start drawing lines
    if(doodle.drawing) {
      doodle.setLocalPen();
      doodle.drawfromto(doodle.oldX, doodle.oldY, x,y);
      doodle.sendLineSeg(doodle.oldX, doodle.oldY, x, y);
    }
    
    // Store the current X, Y position
    doodle.oldX = x;
    doodle.oldY = y;
};

// Finished drawing (mouse up)
doodle.drawEnd = function(ev) {
    doodle.drawing = false;
}

doodle.drawfromto = function(x0, y0, x1, y1) {
    doodle.context.beginPath();
    doodle.context.moveTo(x0, y0);
    doodle.context.lineTo(x1, y1);
    doodle.context.closePath();
    doodle.context.stroke();
}

doodle.setLocalPen = function() {
    doodle.context.strokeStyle = '#000000';
    doodle.linethickness = 2;
}
doodle.setRemotePen = function() {
    doodle.context.strokeStyle = '#404040';
    doodle.linethickness = 2;
}

// Set the drawing method to pen
doodle.pen = function() {
    // Check if pen is already selected
    if($(doodle.penID).hasClass('active')) {
        return;
    }
    // Change color and thickness of the line
    doodle.context.strokeStyle = '#000000';
    doodle.linethickness = 1;
    
    // Flag that pen is now active
    $(doodle.penID).toggleClass('active');
}

doodle.voteSkip = function() {
  doodle.socket.send("VOTESKIP");
}

doodle.submitGuess = function(text) {
  doodle.socket.send("GUESS: "+text);
}
doodle.submitName = function(text) {
  doodle.socket.send("NICK: "+text);
}

doodle.guessBoxKeyup = function(e) {
  switch(e.keyCode) {
      case 13: //Event.KEY_RETURN:
        gb = $(doodle.guessBoxID);
        doodle.submitGuess(gb.val());
        gb.val("");
        break;
  }
}
doodle.nameBoxKeyup = function(e) {
  switch(e.keyCode) {
      case 13: //Event.KEY_RETURN:
        nb = $(doodle.nameBoxID);
        doodle.submitName(nb.val());
        nb.val("");
        break;
  }
}

doodle.saveServer = function(e) {
  nb = $(doodle.serverBoxID);
  var addr = nb.val();
  var testSocket = new WebSocket(addr);
  window.localStorage.setItem('serverAddr', addr);
}
