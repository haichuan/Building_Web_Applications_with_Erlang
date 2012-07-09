CHAPTER 6
WebSockets
Traditionally HTTP is not very good for live communications. The communication
stream is controlled by the client and is really designed for the case where the client
wants to load or set data from time to time. A number of methods have been used to
simulate TCP-socket-like behavior over HTTP but none of them works very well.
HTML5 introduced the idea of WebSockets, a full-on, bi-directional communication
channel between the browser and a server.
In some ways WebSockets take the opposite approach to dealing with interactions
between the browser and the client than REST does. REST is built around the idea that
the browser (or other client) will send a number of discrete requests to the server of the
form, show this data, or perform some action.
As WebSockets are not supported in all browsers, having a
cross-platform way of handling communication would be helpful. This
can be done with the JavaScript package Socket.io (http://socket.io) and
the Erlang package socket.io-erlang (https://github.com/yrashk/socket
.io-erlang).
From an Erlang perspective, WebSockets make interactions between the browser and
Erlang applications more transparent. An Erlang application will generally consist of a
bunch of little servers passing messages around between them. When opening up
WebSockets between the user’s browser and the Erlang application, we can push that
model out onto the user’s browser.
To visualize this, look at Figure 6-1. Here the Erlang cloud on the left consists of a
bunch of processes (shown as squares) passing messages between them (shown as ar-
rows). There is one process shown in orange with rounded corners holding open the
WebSocket (the double arrow), which is talking to the web browser and the client-side
application. By extension there could be many sockets connecting to many web brows-
ers allowing communications between the users.
69
Figure 6-1. Cluster diagram with sockets
Thankfully, while the syntax of how events handlers are called in JavaScript and Erlang
is somewhat different, the semantics are pretty similar. In Erlang we can use a
receive block or an OTP behavior like gen_server or gen_fsm. In JavaScript we can use
the .onmessage event handler and find the data in a dictionary in the parameter list.
However, the web browser does not understand the Erlang message format, so we will
need to translate between the internal messages being sent around by Erlang and a
format that the browser can understand. This can be done with JSON, XML, HTML,
or a custom format. In this chapter, I will use JSON because it is the easiest to work
with in the browser, but other formats can work too if they make sense.
The WebSocket Request
To set up a WebSocket in JavaScript with jQuery, use code as in Example 6-1. The
socket will be opened by creating a WebSocket object with the URL of the server resource.
Once the socket is ready for action, JavaScript will call the socket.onopen handler
function.
Example 6-1. socket.js
$(function ()
{
var WebSocket = window.WebSocket || window.MozWebSocket;
var socket = new WebSocket("ws://localhost:8081/websockets/basic_echo_callback.yaws");
// wait for socket to open
socket.onopen = function ()
{
$('input#echo').on('keypress', function (event)
{
70 | Chapter 6: WebSockets
if ( event.which == 13 ) {
event.preventDefault();
var msg = $(this).val();
});
}
socket.send(JSON.stringify(
{
message:msg
}));
socket.onmessage = function (msg)
{
var message = $.parseJSON(msg.data);
var html
= $('div#messages').html() + message.message + "<br>\n";
$('div#messages').html(html);
});
}
}
As with much of HTML5, not all browsers support WebSockets. As of
this writing, the WebSocket interface is supported by Google Chrome
and Mozilla Firefox, and Microsoft has said that WebSockets will be a
feature of Internet Explorer Version 10. Safari, Opera, and the mobile
browsers do not yet fully support WebSockets. (Opera actually does
support them but only if you explicitly turn them on, which most users
probably don’t.) The Mobile Safari (iOS) and Android browsers also do
not fully support WebSockets. There is also a plug-in for PhoneGap to
allow WebSockets to be used on that platform.
WebSockets are new technology and the specifications for them have
changed a few times, so they should be used with caution at the moment.
I hope that within a few years we will be able to use them more fully.
The Yaws team has been doing a pretty good job of keeping up to date
with the changes, so as long as you keep Yaws up to date you should be
OK.
The JavaScript interface to sockets, then, contains two main functions: a way to send
messages to the server, and a way to handle messages that come back. To send a mes-
sage, use the function socket.send(), which will send a string to the server. In this case
it is the content of the input box, and is triggered when the input box receives a change
event.
To handle incoming messages, use the socket.onmessage handler, which gets called
when the server sends us a message.
Now that we have explored (in brief) the JavaScript interface to how to build a web
socket, it is time to move on to the server side. When the browser opens up a web
socket, it sends a request to the server that looks like a standard HTTP request but with
the addition of an Upgrade header, as in Example 6-2.
The WebSocket Request | 71
Example 6-2. Upgrade header
Upgrade: WebSocket
This header can be found with the is_websocket/1 function, as shown in Exam-
ple 6-3. This will return true if the request is to open a socket and false otherwise.
Example 6-3. Get upgrade header
is_websocket(#headers{other=L}) ->
lists:foldl(fun({http_header,_,K0,_,V}, false) ->
K = case is_atom(K0) of
true ->
atom_to_list(K0);
false ->
K0
end,
case string:to_lower(K) of
"upgrade" ->
true;
_ ->
false
end;
(_, Acc) ->
Acc
end, false, L).
Basic WebSocket Handler
The main conceptual difference between a WebSocket and a normal HTTP connection
is that an HTTP connection is a one-shot item. The request comes in, the server does
something and sends back a response, and that is the end of it. With a socket, the
connection is much more like a persistent TCP socket connection, where multiple
pieces of data are sent back and forth over an extended period of time, as long as several
hours in some cases.1
TheYaws WebSockets interfaces have changed recently. This chapter
works with Yaws Version 1.92, which was released December 23, 2011;
future versions may change things again.
To deal with a WebSocket, a callback module should be defined that exports a function
handle_message/1 (there is also an advanced mode that uses handle_message/2). This
function will be called by Yaws each time the browser sends data over the socket.
If there is no need for the function to save some form of state from one call to the next,
you will need to invoke your socket in advanced mode and do a bit more work to save
1. Technically HTTP also exists over a socket, but it is a short-lived one that is closed as soon as the request
is done, and does not take advantage of much of the power of TCP sockets.
72 | Chapter 6: WebSockets
up partial frames. In that case handle_message/1 should be replaced by handle_message/
2, which has a bunch more options.
The handle_message/1 function should take as an option a tuple in the form {Type,
Data} where Type can be text or binary and Data is the message that is sent. In Exam-
ple 6-4 (which was taken from the Yaws sources) there are several clauses that show
some of the different cases that can occur.
Example 6-4. handle_message/1
-module(basic_echo_callback).
%% Export for websocket callbacks
-export([handle_message/1, say_hi/1]).
handle_message({text, Message}) ->
io:format("~p:~p basic echo handler got ~p~n",
[?MODULE, ?LINE, Message]),
{reply, {text, <<Message/binary>>}}.
say_hi(Pid) ->
io:format("asynchronous greeting~n", []),
yaws_api:websocket_send(Pid, {text, <<"hi there!">>}).
When handle_message/1 is called it can return one of three responses. If it wishes to
reply to the incoming message, it should return {reply, {Type, Data}}, which will send
that message back out to the client.
If handle_message/1 does not have any message to send back, it should return the atom
noreply.
If the server needs to send data to the client not in response to an action by the client,
which is after all one of the main reasons to use a WebSocket, the function yaws_api:web
socket_end/2 as shown in the function say_hi/1 in Example 6-4 will allow that message
to be sent. This can be used in a standard receive loop to allow data from other parts
of an application to be sent to the client.
When the request to establish a WebSocket first arrives to the out/1 function, return
{websocket, CallBackModule, Options} where CallBackModule is the module with han
dle_message/1,2 defined and Options are any initial state that should be passed in (often
just an empty list). A full Erlang implementation of the WebSocket setup code is shown
in Example 6-5. This brings together pieces shown previously for a full picture.
Example 6-5. Setting up a WebSocket
<erl>
get_upgrade_header(#headers{other=L}) ->
lists:foldl(fun({http_header,_,K0,_,V}, undefined) ->
K = case is_atom(K0) of
true ->
Basic WebSocket Handler | 73
atom_to_list(K0);
false ->
K0
end,
case string:to_lower(K) of
"upgrade" ->
true;
_ ->
false
end;
(_, Acc) ->
Acc
end, undefined, L).
%%--------------------------------------------------------------------------------
out(Arg) ->
case get_upgrade_header(Arg#arg.headers) of
true ->
error_logger:warning_msg("Not a web socket client~n"),
{content, "text/plain", "You're not a web sockets client! Go away!"};
false ->
error_logger:info_msg("Starting web socket~n"),
{websocket, basic_echo_callback, []}
end.
</erl>
To close the connection to the client, handle_message/1 can return {close, Reason}.
This echo code in action will look like Figure 6-2. When the browser sends data to the
server, this code is set up to log it as shown in Example 6-6.
Example 6-6. Log from a WebSocket
=INFO REPORT==== 13-Mar-2012::16:43:25 ===
Starting web socket
basic_echo_callback:10 basic echo handler got <<"{\"message\":\"This is a Test\"}">>
Advanced WebSocket Handler
If a process needs more control over the WebSockets or needs to maintain state, using
handle_message/2 will allow the programmer to do that. Contrast the implementation
of handle_message/2 in Example 6-7 with that of handle_message/1 in Example 6-4. In
Example 6-7 the first parameter is a #ws_frame_info record versus the tuple above. This
lets the programmer work with partial frames. The downside is that you must handle
both the state and the partial frames yourself. If being able to handle partial frames is
not something you need, then some form of abstraction could be created to manage
that and just expose the state handling features.
74 | Chapter 6: WebSockets
Figure 6-2. WebSockets in action
Example 6-7. Advanced WebSocket interface
%%%==============================================================
%%% compiled using erlc -I include src/advanced_echo_callback.erl
%%%==============================================================
-module(advanced_echo_callback).
-export([handle_message/2]).
-include("yaws_api.hrl").
Advanced WebSocket Handler | 75
%% define callback state to accumulate a fragmented WS message
%% which we echo back when all fragments are in, returning to
%% initial state.
-record(state, {frag_type = none,
% fragment type
acc = <<>>}).
% accumulate fragment data
%% start of a fragmented message
handle_message(#ws_frame_info{fin=0,
opcode=FragType,
data=Data},
#state{frag_type=none, acc = <<>>}) ->
{noreply, #state{frag_type=FragType, acc=Data}};
%% non-final continuation of a fragmented message
handle_message(#ws_frame_info{fin=0,
opcode=continuation,
data=Data},
#state{frag_type = FragType, acc = Acc}) ->
{noreply, #state{frag_type=FragType, acc = <<Acc/binary,Data/binary>>}};
%% end of text fragmented message
handle_message(#ws_frame_info{fin=1,
opcode=continuation,
data=Data},
#state{frag_type=text, acc=Acc}) ->
Unfragged = <<Acc/binary, Data/binary>>,
{reply, {text, Unfragged}, #state{frag_type=none, acc = <<>>}};
%% one full non-fragmented message
handle_message(#ws_frame_info{opcode=text, data=Data}, State) ->
{reply, {text, Data}, State};
%% end of binary fragmented message
handle_message(#ws_frame_info{fin=1,
opcode=continuation,
data=Data},
#state{frag_type=binary, acc=Acc}) ->
Unfragged = <<Acc/binary, Data/binary>>,
io:format("echoing back binary message~n",[]),
{reply, {binary, Unfragged}, #state{frag_type=none, acc = <<>>}};
%% one full non-fragmented binary message
handle_message(#ws_frame_info{opcode=binary,
data=Data},
State) ->
io:format("echoing back binary message~n",[]),
{reply, {binary, Data}, State};
handle_message(#ws_frame_info{opcode=ping,
data=Data},
State) ->
io:format("replying pong to ping~n",[]),
{reply, {pong, Data}, State};
handle_message(#ws_frame_info{opcode=pong}, State) ->
76 | Chapter 6: WebSockets
%% A response to an unsolicited pong frame is not expected.
%% http://tools.ietf.org/html/\
%%
draft-ietf-hybi-thewebsocketprotocol-08#section-4
io:format("ignoring unsolicited pong~n",[]),
{noreply, State};
handle_message(#ws_frame_info{}=FrameInfo, State) ->
io:format("WS Endpoint Unhandled message: ~p~n~p~n", [FrameInfo, State]),
{close, {error, {unhandled_message, FrameInfo}}}.
In addition, each time handle_message/2 is called in Example 6-7, it is also given a
#state record. This state can then be kept across calls and updated as needed. Thus
handle_message/2 should return {reply, {Type,Data}, State} or {noreply, State}, as
opposed to the forms for handle_message/1 that do not include the State record. 2
To signify that the advanced mode should be used instead of the basic mode, the out/
1 function should return the tuple {websocket, Module, {advanced, InitialState}}.
2. This is very similar to how the OTP gen_server behavior works.
Advanced WebSocket Handler | 77
