CHAPTER 7
Streaming
Sometimes you want to stream data from a server to a client, for example, for an Internet
radio station or a service like Pandora or Ustream. Yaws can do this quite well and with
minimal effort on the part of the programmer.
The difference between streamed data and a standard HTTP connection is that a stream
can remain open for a long period of time (oftentimes hours or days) and send data to
the client for that entire time. However, unlike WebSockets (see Chapter 6) a stream
is a one-way data connection and will normally be binary data like music or video as
opposed to textual data in a WebSocket.
Simple Streaming
To set up streaming in Yaws, the out/1 function should return the tuple {streamcon
tent, MimeType, FirstChunk} as in Example 7-1.
Example 7-1. Setting up streaming (stream.yaws)
<erl>
out(A) ->
io:format("~nStarting audio stream~n"),
spawn(streaming, stream_data, [self()]),
{streamcontent, "audio/mp3", <<>>}.
</erl>
You must also spawn a new process to actually send the data to the client. This is done
in Example 7-1 with the call to spawn/3. This will create a new process and pass the
process ID of the creating process, as shown in Example 7-2. When creating that pro-
cess, the out/1 function passes its own PID via the self/0 function to the function
streaming:stream_data/1.
To
actually
send
the
data
to
the
stream,
call
the
function
yaws_api:stream_chunk_deliver/2 with the Yaws creating PID and the data to be sent.
When the stream is finished, call yaws_api:stream_chunk_end/1 to tell Yaws to close
things down.
79
When streaming audio or video to the HTML5 <audio> and <video> tags,
not all browsers support all formats. So it will be necessary to convert
formats so that all users can see the content if your frontend is HTML5.
If the source of the data is faster than what is receiving the data, replace
yaws_api:stream_chunk_deliver/2 with yaws_api_stream_chunk_deliver_blocking/2.
This will make sure that the data being sent does not overflow the client’s buffers.
Example 7-2. Sending data to a stream (streaming.erl)
-module(streaming).
-export([stream_data/1]).
stream_data(Pid) ->
File
= "audio.mp3",
FileHDL = open_file(File),
stream_from_file(Pid, FileHDL, 1).
open_file(File) ->
{ok, IoDevice} = file:open(File,
[read, binary]),
IoDevice.
stream_from_file(Pid, File, I) ->
Result = file:read(File, 4096),
case Result of
{ok, Data} ->
yaws_api:stream_chunk_deliver_blocking(Pid,Data),
stream_from_file(Pid, File, I+1);
eof ->
yaws_api:stream_chunk_end(Pid);
{error,Reason}->
error_logger:error_msg("~p:~p Error ~p ~n",
[?MODULE, ?LINE, Reason])
end.
Of course, not all audio streams have to be played via a web browser. It is possible to
play audio via a media player like Windows Media Player, iTunes, or VLC. Fig-
ure 7-1 shows an audio stream playing in VLC streamed from Yaws; the code is shown
in Example 7-2.
While this example pulls data from the disk to send to a user for simplicity, it is also
possible to have the data sent from another process that is receiving data from an ex-
ternal source. In that case, you want to change the function stream_from_file/3 in
Example 7-2 to a function that will have a receive block that will get the data.
The great advantage of this is that if you are sending data to a lot of receivers, it is
possible to reduce the memory usage by having one receive loop handle a group of
users. This would make a great deal of sense when data is streaming into an application
in some way (say from an audio input).
80 | Chapter 7: Streaming
Figure 7-1. VLC playing a stream from Yaws
In order to help visualize the flow of data in this application, take a look at Fig-
ure 7-2. In this diagram, data always moves from left to right. Data enters into the
system via the left-most arrow and flows to the line of boxes in the Erlang cloud, which
is a buffer that will send the data out to the streaming processes of the Clients.
When sending large binary messages between processes, Erlang will not
make a copy of the binary but just pass a reference. However, this is
inadvisable to the user.
Figure 7-2. Streaming dataflow diagram
Simple Streaming | 81


