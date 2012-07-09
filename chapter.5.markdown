CHAPTER 5
File Upload
While being able to submit a form or other post variables via Ajax is a useful thing,
sooner or later most applications will want to let a user upload a file, for example an
avatar image or a video.
Yaws allows an application to receive a file from the user in the standard upload format
that users of other server-side technologies such as PHP have come to expect.
When uploading a file in PHP, PHP buffers the input into the /tmp directory and then
sends the finished file to your program. In Yaws, the server sends the file in chunks to
your code. So instead of getting a complete file, the programmer has to be ready to get
a stream of chunks of data. This does put a little more work onto the programmer, but
it also allows the programmer to work with an incomplete upload if that is desired.
The examples in this chapter are taken from the example on the Yaws documentation
website with a few minor changes. The full working code is in Example 5-4, while the
examples preceding it show parts of the code for explanation.
To test uploading, you can of course use a browser, but using the
curl1 program from a command line makes everything easier to test.
Using a command line like this one will upload a file from the local disk
to the Yaws server. In this example, the file you specify should be copied
to the upload directory, /tmp/YawsUploads—if that directory does not
exist it will be created. When the upload is finished, the web server will
return an HTML fragment with the text “File Upload Done”.
curl -F radio=@large_audio_file.mp3 http://localhost:8081/upload.yaws
The File Upload Request
In the case of a file upload, out/1 will be called not just once but multiple times, with
each call having a new slice of the data (see Example 5-1). In order to maintain the state
1. Curl is a standard Unix program and can be run in Windows with Cygwin.
59
of the upload (and anything else that may go along with it) Yaws provides a way to
return a state from out/1 and have it returned to you in the next invocation. In this
example, the state of the upload is encoded in the #upload{} record and stored between
calls in Arg#arg.state.
The first clause in this function uses a guard to check if the Arg#arg.state field is not
set. If it has not been set, then it creates a blank upload object and passes it to multipart/
2. The second clause of the function simply gets the existing state object from
Arg#arg.state and passes it to multipart/2.
Example 5-1. Repeated upload requests
<erl>
multipart(Arg, State) ->
Parse = yaws_api:parse_multipart_post(Arg),
case Parse of
[] -> ok;
{cont, Content, Res} ->
case nextChunk(Arg, Res, State) of
{done, Result} ->
Result;
{cont, NewState} ->
{get_more, Content, NewState}
end;
{result, Res} ->
case nextChunk(Arg, Res, State#upload{last=true}) of
{done, Result} ->
Result;
{cont, _} ->
err()
end
end.
out(A) when A#arg.state == undefined ->
State = #upload{},
multipart(A, State);
out(A) ->
multipart(A, A#arg.state).
</erl>
The function yaws_api:parse_multipart_post/1 will return {result, Res} if this is the
final chunk of data from the browser. However, if the function returns {cont, Contents,
Res} then there is more data to come from the browser. At this point the out/1 function
should return {get_more, Contents, State}. The next time out/1 is called, the State
part of that tuple will be passed back in to be used as shown in Example 5-2.
When the upload is finished, multipart/2 will return a result such as {html, "Upload
Finished"} that will be shown to the user. If the upload is not finished, it will return a
tuple as described above to let Yaws know to give it more data. Note this example does
not save the data that will be shown in “Saving to Disk” on page 61.
60 | Chapter 5: File Upload
Example 5-2. Multipart
multipart(Arg, State) ->
Parse = yaws_api:parse_multipart_post(Arg),
case Parse of
[] -> ok;
{cont, Content, Res} ->
case nextChunk(Arg, Res, State) of
{done, Result} ->
Result;
{cont, NewState} ->
{get_more, Content, NewState}
end;
{result, Res} ->
case nextChunk(Arg, Res, State#upload{last=true}) of
{done, Result} ->
Result;
{cont, _} ->
err()
end
end.
Saving to Disk
The most obvious thing to do with a file upload is to save it to the filesystem. This may
be a final location for the file, or a way to buffer large uploads until they are finished
and can be pushed into some other storage mechanism so as not to use up large amounts
of memory.
To write a file to disk call the BIF file:open/2 as in Example 5-3. If there is not an error,
this will return {ok, FD} where FD is the file handle that can be used to write to the file.
For full details on handling files in Erlang, see the Erlang manual pages.
Once the file has been opened, each subsequent chunk of data can be added to the file
with file:write/2 until the end of the file, when file:close/2 can be called to close
the file handle.
If a process dies in the middle of writing a file, Erlang will close the file
handle automatically. It may be worth it to have the monitoring mech-
anism delete the file as well.
There are several clauses of the writeToDisk/3 function in Example 5-3, but they all
take the same three parameters. The first is the standard Arg record that Yaws sends to
out/1, which is passed on here. The second is a list of the parts of the file to be saved,
and the third is the current state record.
The parts buffer is a list of chunks of the uploaded file that can be saved to disk. If the
list is empty and State#upload.last is false, then all the data that has been buffered has
Saving to Disk | 61
been processed. In this case writeToDisk/3 will return {cont, State}, which will let
Yaws know to send the next chunk of data when it arrives and wait until that happens.
When the buffer is not empty, it will consist of a list of tuples of the form {Atom,
Data}. There are several possible atoms that could be sent.
The first element to be sent will be sent with the form {head, {Name, Options} }. To
handle this, writeToDisk/3 should open the file handle, set up the state record, and then
recursively call writeToDisk/3 with the new state record and the tail of the buffer list.
In the case of a chunk of data in the middle of a file, the head of the buffer will look
like {body, Data}. In this case, the data should be written out to disk, and then write
ToDisk/3 should again be called recursively with the tail of the list.
If the buffer list is empty and State#upload.last is true, then the file is finished up-
loading. At this point we can call file:close/1 to close the file handle. After that we
can call upload_callback/1 to handle any operations that we may wish to handle after
the upload finishes (such as syncing to other nodes or uploading to CouchDB) and we
return a done status.
Example 5-3. Save file upload
writeToDisk(A, [{part_body, Data}|Res], State) ->
writeToDisk(A, [{body, Data}|Res], State);
writeToDisk(_A, [], State) when State#upload.last==true,
State#upload.filename /= undefined,
State#upload.fd /= undefined ->
file:close(State#upload.fd),
upload_callback(State),
Res= {html, "Done"},
{done, Res};
writeToDisk(A, [], State) when State#upload.last==true ->
{done, err()};
writeToDisk(_A, [], State) ->
{cont, State};
writeToDisk(A, [{head, {_Name, Opts}}|Res], State ) ->
case lists:keysearch(filename, 1, Opts) of
{value, {_, Fname0}} ->
Fname = yaws_api:sanitize_file_name(basename(Fname0)),
TargetDir = "/tmp",
file:make_dir(TargetDir),
case file:open([TargetDir, Fname] ,[write]) of
{ok, Fd} ->
S2 = State#upload{filename = Fname,
fd = Fd},
writeToDisk(A, Res, S2);
Err ->
{done, err()}
62 | Chapter 5: File Upload
end;
false ->
end;
writeToDisk(A,Res,State)
writeToDisk(A, [{body, Data}|Res], State)
when State#upload.filename /= undefined ->
case file:write(State#upload.fd, Data) of
ok ->
writeToDisk(A, Res, State);
Err ->
{done, err()}
end.
If uploading files is a large part of an application, then the disk can become a bottleneck
in the application’s performance. While the server may have 20 or 40 cores, the disk is
very sequential and the slowest part of the system. This has to be considered in light
of Amdahl’s law (see “Amdahl’s law” on page 8). It’s possible that using something
like Amazon’s S3 might be a better solution (see “Saving to Amazon S3”
on page 66).
Putting It All Together
Example 5-4 brings together the various pieces of code to show you how to upload a
file in Yaws.
Example 5-4. Complete upload code (upload.yaws)
<erl>
-record(upload, {
fd,
filename,
last}).
-define(DIR, "/tmp/").
out(Arg) when Arg#arg.state == undefined ->
State = #upload{},
multipart(Arg, State);
out(Arg) ->
multipart(Arg, Arg#arg.state).
err() ->
{ehtml,
{p, [], "error"}}.
multipart(Arg, State) ->
Parse = yaws_api:parse_multipart_post(Arg),
case Parse of
[] -> ok;
Putting It All Together | 63
{cont, Cont, Res} ->
case addFileChunk(Arg, Res, State) of
{done, Result} ->
Result;
{cont, NewState} ->
{get_more, Cont, NewState}
end;
{result, Res} ->
case addFileChunk(Arg, Res, State#upload{last=true}) of
{done, Result} ->
Result;
{cont, _} ->
err()
end
end.
addFileChunk(Arg, [{part_body, Data}|Res], State) ->
addFileChunk(Arg, [{body, Data}|Res], State);
addFileChunk(_Arg, [], State) when State#upload.last
== true,
State#upload.filename
/= undefined,
State#upload.fd
/= undefined ->
file:close(State#upload.fd),
Res = {ehtml,
{p,[], "File upload done"}},
{done, Res};
addFileChunk(Arg, [], State) when State#upload.last==true ->
{done, err()};
addFileChunk(_Arg, [], State) ->
{cont, State};
addFileChunk(Arg, [{head, {_Name, Opts}}|Res], State ) ->
case lists:keysearch(filename, 1, Opts) of
{value, {_, Fname0}} ->
Fname = yaws_api:sanitize_file_name(basename(Fname0)),
%% we must not put the file in the
%% docroot, it may execute uploade code if the
%% file is a .yaws file !!!!!
file:make_dir(?DIR),
case file:open([?DIR, Fname] ,[write]) of
{ok, Fd} ->
S2 = State#upload{filename = Fname,
fd = Fd},
addFileChunk(Arg, Res, S2);
Err ->
{done, err()}
end;
false ->
addFileChunk(Arg,Res,State)
end;
64 | Chapter 5: File Upload
addFileChunk(Arg, [{body, Data}|Res], State)
when State#upload.filename /= undefined ->
case file:write(State#upload.fd, Data) of
ok ->
addFileChunk(Arg, Res, State);
Err ->
{done, err()}
end.
basename(FilePath) ->
case string:rchr(FilePath, $\\) of
0 ->
%% probably not a DOS name
filename:basename(FilePath);
N ->
%% probably a DOS name, remove everything after last \
basename(string:substr(FilePath, N+1))
end.
</erl>
Storage in a Distributed System
The other complication is that writing the file to disk is probably not the correct way
to handle the data. Erlang applications are distributed applications that run across a
large number of servers. So if you upload a file from a user and it gets put on one node,
it will not be seen by all the others. In this case it is a much better idea to keep the file
in some sort of data store that has a way of replicating data around the network.
One solution is to try to put files on a shared filesystem. Unless it’s a system like Am-
azon’s S3, however, this can be a bad idea for a few reasons. First of all, the server that
holds that system will become a bottleneck and a single point of failure. If that system
were to go offline, the entire system will become unavailable. In addition, such a system
would have to be quite large to handle the load of all the clients. Once again, the
specifics of storage will have to be evaluated in light of the design and use of the
application.
Using something like CouchDB (see “CouchDB” on page 12) would make sense here
as it will allow the file to be propagated around the nodes of the application pretty well.
In this case, what would probably happen is that the file would be uploaded to the local
disk and then, when the upload is complete, it would be moved into the distributed
system, be that CouchDB, Riak, HBase, or something else. This way, if a file upload is
canceled or is corrupt, it will not be propagated out onto the network.
The other option for dealing with uploaded data is not to write it out at all, but to
stream it to the users. Yaws is fully able to stream multimedia—see the Yaws docu-
mentation for more detail.
Storage in a Distributed System | 65
Saving to Amazon S3
Often we will want to take a file that a user has uploaded and make it available to the
world to download. For example, think of a video on YouTube: the user uploads the
file, probably does some manipulation of the data itself (converting formats, etc.), and
then puts it somewhere that other users can view it.
One of the easiest ways to do this is to save the file to Amazon S3, a highly reliable
cloud service that was built to solve this particular problem.
To use Amazon Web Services (AWS) from Erlang, use the erlcloud package at https://
github.com/gleber/erlcloud. This package provides an Erlang interface to AWS. In this
case we’re interested only in the S3 service.
In Amazon S3 files live in buckets, and the code in Example 5-5 assumes that we have
created a bucket already. The name of the bucket should be set by the -define() in the
file. It is also possible to set default options on the bucket so that they are what your
application needs. In addition, there are two keys that have to be set in this file (set
them to your AWS keys).
Once the file has been uploaded from the user we need to upload it to S3 using the
function erlcloud_s3:put_object/6 (if you want to allow default options there are also
functions erlcloud_s3:put_object/3-5). Pass this function the bucket name, the key,
and the value to upload; you can also pass options, HTTP headers, and a config object.
This will upload the object to Amazon S3.
Once everything is set, we can upload a file to S3. To do this we pass in the key and
value to the function s3:upload(), which will call erlcloud_s3:put_object/3 to upload
the file.
If the file is on disk use the function s3:upload_file/2, which will automatically read
the file into memory and pass it on to upload/2.
Example 5-5. Uploading to S3 (s3.erl)
-module(s3).
-define('ACCESS_KEY',
-define('SECRET_ACCESS_KEY',
-define('BUCKET',
"********************").
"****************************************").
"*************").
-export([upload/2, upload_file/2]).
upload_file(Key, Path) ->
{ok, Binary} = file:read_file(Path),
upload(Key, Binary).
upload(Key, Value) ->
erlcloud_ec2:configure(?ACCESS_KEY, ?SECRET_ACCESS_KEY),
error_logger:info_msg("~p:~p Settng up AWS to S3 ~n",
66 | Chapter 5: File Upload
[?MODULE, ?LINE]),
R = erlcloud_s3:put_object(?BUCKET, Key, Value, [], [{"Content-type", "image/jpeg"}]),
error_logger:info_msg("~p:~p Uploaded File ~p to S3 ~n",
[?MODULE, ?LINE, R]),
{ok, R}.
Obviously, before using this example you will need to fill in your access key and secret
key as well as the name of your bucket. In addition, before trying to upload code to S3
you will need to start up the inets and ssl services in Erlang. To do that, run these two
lines when starting the Erlang node:
inets:start().
ssl:start().
To see this in action you can run Example 5-6, which will take a key and file from a
Unix command line and upload it to S3. There are better AWS command-line tools,
but this is a helpful way of testing the code in Example 5-5.
Example 5-6. Uploading to S3 shell wrapper (s3_upload)
#!/usr/bin/env escript
-export([main/1]).
main([Key, File_Name]) ->
inets:start(),
ssl:start(),
s3:upload_file(Key, File_Name).
Saving to Amazon S3 | 67
