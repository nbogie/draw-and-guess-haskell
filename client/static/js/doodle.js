/* ------------------------------------------------------------------------
    Title:          Draw and Guess Client
    
    Description:  A client for a websockets-based drawing and guessing game.
       When connected to a suitable server, each client is assigned a
       team and role.  As an artist, the player must draw a provided word.
       As a guesser, the player must guess at what the artist is drawing, 
       as it appears on his screen, before the other team do the same.

    Authors: Neill Bogie, Andrew Mason

    Credits: 
        Based heavily upon v0.2 of Tiny Doodle http://tinydoodle.com/
        by Andrew Mason (a.w.mason at gmail dot com) http://analoguesignal.com/
    
    Requirements:
        * Jquery 1.3+
    
    
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
    var host = "50.56.82.241";
    var serverURL = 'ws://' + host + ':12345';
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
    } else if ((/^\{\"mteams\"/).test(event.data)) {
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
      if (rightness=="CORRECT") {
        $(doodle.noticeID).prepend("<li>"+who + " guessed <b>" + guess + "</b> ✔</li>");
      } else {
        $(doodle.noticeID).prepend("<li>"+who + " guessed <b><del>" + guess + "</del></b> ✘</li>");
      }
    } else if ((/^STATE /).test(event.data )) {
      parts = event.data.split(" ");
      var st = parts[1];
      $('#playstate').html(st);
    } else if ("ROUNDSTART" == event.data ) {
      console.log("Round starts!");
      doodle.clearCanvas();
    } else if ((/^CORRECT_GUESS_BY_YOU$/).test(event.data) ) {
      console.log("Server says this client's guess was correct.  Cue fireworks.");
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
  console.log("in update Teams");
  for (var tix = 0; tix < teamsData.mteams.length; tix++) {
    var t = teamsData.mteams[tix];
    var sel = '#team'+t.teamId+'score';
    $(sel).html(""+t.mscore);
    for (var pix = 0; pix < t.members.length; pix++) {
      var p = t.members[pix];
      console.log("name: "+ p + " artist name " + t.martist);
      var isArtist = p==t.martist;
      var cl = isArtist ? "artist" : "";
      $('#team'+t.teamId+'list').append("<li class='"+ cl +"'>"+p+"</li>");
    }
  }
}

var doodle = {
    // Define some variables
    'drawing':          false,
    'linethickness':    2,
    'voteSkipID':       '#voteskip',
    'guessBoxID':       '#guessBox',
    'nameBoxID':        '#nameBox',
    'serverBoxID':      '#serverBox',
    'saveServerID':     '#saveServer',
    'noticeID':         '#notification'
};

doodle.init = function(givenSocket) {
    doodle.socket = givenSocket;
    // Collect elements from the DOM and set-up the canvas
    doodle.canvas = $('#doodle_canvas')[0];
    doodle.context = doodle.canvas.getContext('2d');
    
    doodle.clearCanvas();
    
    // Mouse based interface
    $(doodle.canvas).bind('mousedown', doodle.drawStart);
    $(doodle.canvas).bind('mousemove', doodle.draw);
    $(doodle.canvas).bind('mouseup', doodle.drawEnd);
    $('body').bind('mouseup', doodle.drawEnd);
    
    // Touch screen based interface
    $(doodle.canvas).bind('touchstart', doodle.drawStart);
    $(doodle.canvas).bind('touchmove', doodle.draw);
    $(doodle.canvas).bind('touchend', doodle.drawEnd);
    
    //Not really doodle stuff
    $(doodle.voteSkipID).bind('click', doodle.voteSkip);
    $(doodle.guessBoxID).bind('keyup', doodle.guessBoxKeyup);
    $(doodle.nameBoxID).bind('keyup', doodle.nameBoxKeyup);
    $(doodle.saveServerID).bind('click', doodle.saveServer);
};


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
    
    // Set the drawning method to pen
    doodle.setLocalPen();
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
    doodle.context.strokeStyle = '#000000';
    doodle.linethickness = 2;
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
