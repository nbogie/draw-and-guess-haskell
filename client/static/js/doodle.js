/* ------------------------------------------------------------------------
    Title:          Tiny Doodle
    
    Version:        0.2
    URL:            http://tinydoodle.com/
    
    Description:
        Tiny Doodle is an exercise in learning about <canvas>.
        Event handlers are attached the to <canvas> elemet for both
        mouse and touch input devices. The user can doodle away on the
        <canvas>, clear and save the resulting doodle.
        
        Saving the doodle extracts the canvas data in base64 format,
        POST's the string to a Python service which stores it in a 
        database.
    
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

    var socket = new WebSocket('ws://localhost:12345');
    doodle.init(socket);

    socket.onopen = function(event) {
      socket.send('HELLO');
    };

    socket.onmessage = function(event) { 
    if ((/ DRAW /).test(event.data )) { //TODO: tighten format
      parts = event.data.split(" ");
      //console.log("GOT DRAW: " + event.data);
      var x0 = parseInt(parts[2]);
      var y0 = parseInt(parts[3]);
      var x1 = parseInt(parts[4]);
      var y1 = parseInt(parts[5]);
      doodle.setRemotePen();
      doodle.drawfromto(x0, y0, x1, y1);
    } else if ((/\{\"mteams\"/).test(event.data)) {
        console.log("updating teams with: " + event.data);
        updateTeams(JSON.parse(event.data));//TODO: security.  only parse what you trust 100%
        parts = event.data.split(" ");
        //console.log("GOT DRAW: " + event.data);
        var x0 = parseInt(parts[2]);
    } else if ("ROUNDSTART" == event.data ) {
      console.log("Round starts!");
      doodle.newDoodle();
    } else if ("ok" == event.data ) {
    } else {
      console.log("GOT WS MSG: " + event.data);
      $(doodle.noticeID).prepend("<li>GOT: " + event.data + "</li>");
    }};

    socket.onclose = function(event) { alert('The websocket to the game server is closed.'); }

    socket.onerror = function(event) { alert('An error occurred with the game websocket: ' + event + " Data: " + event.data); }

});

updateTeams = function(teamsData) {
  $('#team0list').empty();
  $('#team1list').empty();
  for (var tix = 0; tix < teamsData.mteams.length; tix++) {
    var t = teamsData.mteams[tix];
    console.log("TEAM " + t.teamId + " score: "+t.mscore);
    var sel = '#team'+t.teamId+'score';
    $(sel).html(""+t.mscore);
    for (var pix = 0; pix < t.members.length; pix++) {
      var p = t.members[pix];
      console.log("member:  " + p);
      $('#team'+t.teamId+'list').append("<li>"+p+"</li>"); //TODO: security.  vet member representation
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
    'guessboxID':       '#guessbox',
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
    $(doodle.guessboxID).bind('keyup', doodle.guessboxKeyup);
};

doodle.loadDoodles = function(cookie) {
    var keys = cookie.split(",");
    for (var i = 0; i < keys.length; i++) {
        doodle.newDoodle('/thumb?id='+keys[i]+'&rnd='+Math.random(), keys[i]);
    }
}

doodle.saveImage = function(ev) {
    // Extract the Base64 data from the canvas and post it to the server
    base64 = doodle.canvas.toDataURL("image/png");
    if(!doodle.updating) {
        $.post('/save', {img: base64}, function(data) {doodle.updateThumb(data)});
    } else {
        $.post('/save', {img: base64, key: doodle.loaded_id}, function(data) {doodle.updateThumb(data)});
    }
}

doodle.updateThumb = function(data) {
    // Notify the user that the image has been saved
    //$(doodle.noticeID).html('Saved');

    var thumb = $('img.active');
    // Reset the thumb image
    // Note: a random number is added to the image to prevent caching
    thumb.attr('src', '/thumb?id='+data+'&rnd='+Math.random());
    thumb.attr('id', 'i'+data);
    $('img.active').bind('click', doodle.loadImage);
    
    // Save doodle ID to a cookie
    if (doodle.loaded_id !== data) {
        var keys;
        if ($.cookie('doodles')) {
            keys = $.cookie('doodles') + ',' + data;
        } else {
            keys = data;
        }
        $.cookie('doodles', keys);
    }
    
    // Store doodle ID
    doodle.loaded_id = data;
    
    // The doodle has been saved, update from here on
    doodle.updating = true;
}

doodle.newDoodle = function(src, id) {
    doodle.clearCanvas();
    if (!src) {
        src = 'static/images/blank.gif';
    }
    
    if (!id) {
        id = '';
    }
    // Build an empty thumb
    thumb_html = '<img class="active" src="'+src+'" id="i'+id+'" width="32" height="24" />';

    // Add the thumb to the DOM then bind click event
    $('#output').append(thumb_html);
    $('#output img').bind('click', doodle.loadImage);
    //$('img.active').bind('click', doodle.loadImage);
}

doodle.loadImage = function(event) {
    // Stop from following link
    event.preventDefault();
    
    // If the current doodle is loaded, do nothing
    if ($(this).hasClass('active')) {
        return;
    }
    
    // Clear the canvas
    doodle.clearCanvas();
    
    // Load saved image onto the canvas
    if ($(this).attr('id')) {
        doodle.loaded_id = $(this).attr('id').slice(1);
        var img_src = '/image?id=' + doodle.loaded_id + '&rnd='+Math.random();
        var img = new Image();
        img.src = img_src;
        
        // Wait for image to finish loading before drawing to canvas
        img.onload = function() {
            doodle.context.drawImage(img, 0, 0);
        };
        
        // Flag that user is updating a saved doodle
        doodle.updating = true;
    } else {
        
    }
    
    
    // Add active class to selected thumb
    $(this).addClass('active');  
    


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
  doodle.socket.send("DRAW "+x0 + " "+y0+" "+x1+" "+y1);
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
  console.log("FAKE: would submit guess " + text);
  doodle.socket.send("GUESS: "+text);
}

doodle.guessboxKeyup = function(e) {
  switch(e.keyCode) {
      case 13: //Event.KEY_RETURN:
        gb = $(doodle.guessboxID);
        doodle.submitGuess(gb.val());
        gb.val("");
        break;
  }
}

