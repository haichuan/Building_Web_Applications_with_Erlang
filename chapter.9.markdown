CHAPTER 9
Building an Application with OTP
So far this book has shown small pieces of Erlang or other code to demonstrate one
idea or another. This chapter does something a bit different. Here I will develop a larger
application to demonstrate how all the parts of an Erlang- and Yaws-based web appli-
cation hang together.
This application will allow a bunch of users to notify each other of updates in status.
Whenever a user’s status changes in their browser or on their phone, that change will
be made available by the server. It will also keep track of each user’s status so when a
user signs in, she can see all the existing status messages. This application can serve as
the base of many distributed applications, and I hope it will prove illustrative of how
to build an application in Erlang.
Feel free to use this module as a basis for your own product. If you do
something really cool with it, please let me know!
This application will also split the application into layers: we’ll have a server level that
will coordinate between the users, and a web frontend that will use a simple web
interface.
In this chapter we’ll build a more complex application using the standard Erlang/OTP
structures. By doing this we can take advantage of the fact that OTP is a very well tested
framework for building extremely robust servers, and match that with an interface in
Yaws that can work with the web browser.
An OTP application features several parts, all of which must be present. First we have
the workers that actually perform the tasks of the application—in this case, keeping
track of user status messages.
But beyond our workers we also have some other processes. The first type is a super-
visor. The supervisor exists to keep an eye on the workers—if a worker dies, the
91
supervisor will restart the process, and the users will simply see a restarted server the
next time they try to poll for a status.
To understand how this works, look at Figure 9-1. In this diagram each box represents
a process and each process is responsible for those below it on the tree. Here the su-
pervisors are depicted as squares while the workers are circles. If one of the processes
were to die (which sooner or later will happen), its supervisor will restart it. The full
setup does not have to be on one server, so it would be possible for the nodes on the
right to be on one server while those on the left are on the other, thus giving us the
ability to handle fallover. How to do that fully is beyond the scope of this book.
Figure 9-1. Supervision tree
Directory Structure
The OTP application wants files to be in a standard set of directories. These are ebin,
include, priv, src, and test. Other directories can be added to this if needed.
The ebin directory is the only one that is actually required, and it should hold
the .beam files as well as the .app file that describes the application (see “The App
File” on page 115).
The src directory will hold all Erlang sources required for the applications. Any .hrl
include files should be in the include directory. Testing code of course lives in the test
directory.
The final directory is priv, which can contain any other resources that an application
may need. This can include templates, config files, so forth. You can always get the
application’s priv directory by calling code:priv_dir(Application).
92 | Chapter 9: Building an Application with OTP
Building an Application Server
When building an application in Erlang, I like to start by thinking about what kind of
information is moving around the application. I will normally diagram this with pencil
and paper or on a whiteboard. However, to save you the trouble of deciphering my
handwriting I have translated it to a more readable form (see Figure 9-2).
Figure 9-2. Data movement
This web application consists of a few parts, so let’s look at it from the outside in. There
is a part that runs in the web browser (covered in “Interfacing the Server with the
Web” on page 101) that will communicate with the server via a web resource. The
browsers are the right-hand squares on the right, the Yaws web servers are the rounded
boxes in the middle, and the OTP server is the gen_server on the left of the diagram.
In this case we use a generic server as shown in “The Multicast Server” on page 96 to
hold the state of each user. Users will update their status by periodically polling for
changes over a web interface.
The Generic Server
The actual logic of an OTP application will consist of a collection of basic servers that
will normally be written around the basic abstractions provided by the OTP library.
The main one is gen_server, which is a generic server. The gen_server provides all the
standard parts of a server, and all the developer has to do is create the functions that
implement the features needed for the application in question.
Much like implementing an interface in Java, to implement gen_server a module must
implement a few functions: start_link/0, init/1, handle_call/3, handle_cast/2, han
dle_info/2, terminate/2, code_change/3. If you use Emacs as your editor, Erlang mode
The Generic Server | 93
will create this structure along with a number of others for you with a template. For an
example of this template, take a look at Example D-3 in Appendix D. Many of the
examples in this chapter are based on the Emacs templates.
To understand all of this, it helps to have a simple module to look at. Example 9-1 is a
simple server that will generate sequential unique IDs.
Example 9-1. Generate unique IDS (uniq.erl)
-module(uniq).
-behaviour(gen_server).
%% API
-export([start_link/0]).
-export([get_id/0]).
%% gen_server callbacks
-export([init/1,
handle_call/3,
handle_cast/2,
handle_info/2,
terminate/2,
code_change/3]).
-define(SERVER, ?MODULE).
-record(state, {count}).
get_id() ->
{id, ID} = gen_server:call(?MODULE, {}),
ID.
%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) ->
{ok, #state{count= 1 }}.
handle_call(_Request, _From, State) ->
Count = State#state.count,
{reply,
{id, Count},
#state{count = Count + 1}}.
handle_cast(_Msg, State) ->
{noreply, State}.
handle_info(_Info, State) ->
{noreply, State}.
terminate(_Reason, _State) ->
94 | Chapter 9: Building an Application with OTP
ok.
code_change(_OldVsn, State, _Extra) ->
{ok, State}.
This server is started by calling start_link/0, which will initialize the server by calling
the init/1 function. This always starts off the internal state of the server with count
being 1.
This module exports one function API that consists of the function get_id/0. This
function uses the gen_server:call/2 function to send a message requesting an ID from
the server. Unlike using something like Pid ! get_id, this has an implicit timeout. If
the server does not respond inside five seconds, gen_server:call/2 will die and leave a
message. If this is not enough time (or too much), you can pass a third parameter that
will let you specify a timeout. Pass the time in milliseconds or the atom infinity.
The handle_call function will be called when a message comes in that has to be re-
sponded to. In general this function will do something and return a tuple like {reply,
Reply, NewState}.
If you have programmed in Haskell or a similar language, you may notice
that the gen_server looks a lot like a state monad.
All of the state that is used by the functions in the server is bound up in the State
parameter that is passed in to the various functions; there are no other artifacts such
as singletons in Java or the JavaScript window object. This may seem quite restrictive,
but it actually enables some very powerful features.
This also makes writing tests much easier! The handle_call function
will normally be close to a pure function, with all the global state in one
place both before and after. There is no chance for a strange side effect
to go clobber something over there.
The OTP framework wraps messages that are sent in a structure that enables the
gen_server to call both handle_cast and handle_call for different kinds of messages.
However, if a message is sent to the server that is not wrapped by the gen_server
framework it will be handled by handle_info/2.
If you do not need this functionality, then having a handle_info/2 function that will log
any messages as errors will enable you to track down where they are being sent from.
Or you could omit the handle_info/2 function altogether; you will get a warning when
you compile but it can be ignored. In this case, when an unknown message is sent to
the process gen_server will terminate the process and leave an error message. In general,
this is what you want as the supervisor will recreate it (see “Let’s Have Some Adult
The Generic Server | 95
Supervision Around Here!” on page 104). Remember, in Erlang, defensive program-
ming is bad—we want a server that is going wrong to terminate so that the supervisor
can start up a fresh copy with a known good state.
If your server needs to do any cleanup when it is done, use the terminate/2 function.
This can close database connections or flush buffers, remove temporary files, and so
on. The first parameter of the terminate/2 function will be the reason the process is
being terminated—it could be because the supervisor was told to shut down, or is
shutting down a number of workers, or a linked node is shutting down.1
The Multicast Server
In our application (Example 9-2) we have a fairly simple server implementation in
multi_cast_server. This server keeps track of the most recent state of each user. As
such, this function only has an external API of three functions: get_current_user_sta
tus/0, which gets the status of all users on the system; get_current_user_status/1,
which gets the status of a specific user; and update_status/2, which updates the user’s
status.
Each of these functions will send a message via the gen_server:call/2 function to the
server, which will reply with a clause of the handle_call/3 function.
The handle_call/3 function takes the three possible requests and the current state of
the server and either updates the state or returns the requested information.
This server has more logging in it than you might normally want. I like
to put a lot of log statements in for development.
The actual state of the group is passed around as a State value to each element. In this
case it is just an array, though in a more complex example it could be (and probably
should be) a record for a more complex data structure. There are two functions that
have been created to deal with this state: update_user_status/3 and get_user_status/2.
Example 9-2. Multicast server
%%%-------------------------------------------------------------------
%%% @author Zach Kessin <>
%%% @copyright (C) 2012, Zach Kessin
%%% @doc
%%%
%%% @end
%%% Created : 21 Mar 2012 by Zach Kessin <>
1. See the manual page for the full details (http://www.erlang.org/doc/man/gen_server.html#Module:
terminate-2).
96 | Chapter 9: Building an Application with OTP
%%%-------------------------------------------------------------------
-module(multi_cast_server).
-behaviour(gen_server).
%% API
-export([start_link/0]).
-export([get_current_user_status/0,
get_current_user_status/1,
update_status/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
%%%===================================================================
%%% API
%%%===================================================================
get_current_user_status() ->
gen_server:call(?MODULE, {get_current_user_status}).
get_current_user_status(User) ->
gen_server:call(?MODULE, {get_current_user_status, User}).
update_status(User, Status) ->
ok = gen_server:call(?MODULE, {update_status, User, Status}),
ok.
%%%===================================================================
%%% Functions for internal Use
%%%===================================================================
update_user_status([], User, Status) ->
[{User, Status}];
update_user_status([{User, _OldStatus} | Tail], User, Status) ->
[{User,Status} | Tail];
update_user_status([{O,S}|Tail], User, Status) ->
R = update_user_status(Tail, User, Status),
[{O,S}|R].
get_user_status(UserStatus, TargetUser) ->
case lists:filter(fun({User,_Status}) ->
User == TargetUser
end,
The Multicast Server | 97
UserStatus) of
[] ->
no_status;
[TargetUserStatus] ->
{ok, TargetUserStatus}
end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%
{ok, State, Timeout} |
%%
ignore |
%%
{stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
io:format("~n~p:~p(~p) init(~p)~n",
[?MODULE, ?LINE, self(), []]),
{ok, []};
init(Status) ->
io:format("~n~p:~p(~p) init(~p)~n",
[?MODULE, ?LINE, self(), Status]),
{ok, Status}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%
{reply, Reply, State} |
%%
{reply, Reply, State, Timeout} |
%%
{noreply, State} |
98 | Chapter 9: Building an Application with OTP
%%
{noreply, State, Timeout} |
%%
{stop, Reason, Reply, State} |
%%
{stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_current_user_status}, _From, State) ->
{reply,
{ok, State},
State};
handle_call({get_current_user_status, User}, _From, State) ->
{reply,
get_user_status(State, User),
State};
handle_call({update_status, User, Status}, _From, State) ->
io:format("~p:~p (~p) Update ~p -> ~p ~n",
[?MODULE, ?LINE, self(), User, Status]),
io:format("STATE ~p ~n", [State]),
NewState = update_user_status(State, User, Status),
{reply, ok, NewState}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%
{noreply, State, Timeout} |
%%
{stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
{noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%
{noreply, State, Timeout} |
%%
{stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
{noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
The Multicast Server | 99
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
ok.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
{ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
When developing this module I first exposed those two functions with an -export()
directive and made sure that they did the right thing on test data by trying out a number
of test cases. Once I was sure that these two functions worked as they should, I removed
the export module and started up the server. I then tried a number of examples from
the Erlang command line, as summarized in Example 9-3. In fact I literally cut and
pasted this code from an Emacs buffer into Yaws, which was running in an Emacs shell
buffer. It crashed my server with an error, I fixed that bug, and repeated until everything
worked.
Example 9-3. Multicast server test
c(multi_cast_server).
multi_cast_server:start_link().
multi_cast_server:update_status("Zach","Testing").
multi_cast_server:update_status("Nati","TV").
multi_cast_server:update_status("Zach","Coding").
multi_cast_server:get_current_user_status("Zach").
multi_cast_server:get_current_user_status().
In addition to the generic server (gen_server), there is also a generic finite
state machine (gen_fsm) implementation and a lot more in the OTP
framework. These are beyond the scope of this book, but Learn You
Some Erlang and the Erlang documentation cover them quite well.
100 | Chapter 9: Building an Application with OTP
Interfacing the Server with the Web
So far we have a bunch of Erlang services that are kind of interesting but not particularly
useful in and of themselves, as they can’t interface with the outside world—which is,
after all, what we want to do. So we need to write some code to provide a web interface
onto all of this.
To do this we will first add an htdocs directory to the standard set of Yaws directories.
This directory can contain all the public facing files of the web service includ-
ing .yaws files, images, CSS, JavaScript, and so forth.
In this case we will connect the htdocs dir to the Yaws document root
via a symlink, but we could also do it in the yaws.conf file. It is also
possible to run Yaws as an OTP app inside an existing Erlang setup, but
this is beyond the scope of this book.
The first file, shown in Example 9-4, is a simple one that calls the
multi_cast_server:get_current_user_status/0 function and then formats the result as
a JSON for the server with the function convert_to_json/1. Note that the strings are
converted to binaries with the list_to_binary/1 function that is built into Erlang. If
you don’t do this then you will get back an array of integers in the JSON, which is
probably not what you had in mind.
Example 9-4. Get status (status.yaws)
<erl>
convert_to_json(Data) ->
Content = [{obj, [{name,
list_to_binary(Name)},
{status, list_to_binary(Status)}]} ||
{Name, Status} <-Data],
{obj, [{data, Content}]}.
out(_Arg) ->
{ok, Obj} = multi_cast_server:get_current_user_status(),
io:format("~n (~p) Raw Data ~p~n", [self(), Obj]),
JSON = rfc4627:encode(convert_to_json(Obj)),
io:format("~n (~p) JSON -> ~p~n", [self(), JSON]),
{content, "application/json", JSON}.
</erl>
Once again we have some extra log information in this example; the output from the
logs can be found in Example 9-5. Here you can see the raw data that comes back from
the server, and the JSON into which it has been converted (extra whitespace has been
added).
Interfacing the Server with the Web | 101
Example 9-5. Get status log data
(<0.365.0>) Raw Data [{"Zach","Coding"},{"Nati","TV"}]
(<0.365.0>) JSON ->
"[{\"name\":\"Zach\",\"status\":\"Coding\"},{\"name\":\"Nati\",\"status\":\"TV\"}]"
The users can get the status of other users by sending a GET to Example 9-4. This is a
very simple .yaws file that serves only to call the server and then translate the returned
data into the JSON that the client will expect.
To set the status of a user, the browser will send a POST request to “set-status.yaws”
(Example 9-6). As above, this file contains only enough code to decode the user’s re-
quest and pass the data on to the server.
Example 9-6. Set status (set-status.yaws)
<erl>
out(Arg) ->
{ok, Name}
= postvar(Arg, "name"),
{ok, Status} = postvar(Arg, "status"),
io:format("~n(~p) Name ~p, Status ~p ~n",
[self(), Name, Status]),
multi_cast_server:update_status(Name, Status),
{html, "true"}.
</erl>
Some Client-Side Code
For the client-side code we are going to keep it very simple. Using ExtJS we will con-
struct a simple interface that will show the current status of all the users in a grid. At
the bottom of the grid will be a field where users can enter their current status.
In Figure 9-3, the browser application displays the current status of the various users
with an interface written in ExtJS. The CoffeeScript code in Example 9-7 shows a basic
interface of a grid displaying each user along with their status.
The interface also has a form allowing the user to set his or her status. This example
lets you set the status for any user; a more robust example should of course use some
authentication to determine the user.
Example 9-7. Socket handler (socket_handler.coffee)
makeStore = ->
store = Ext.create("Ext.data.Store",
autoLoad : true
fields
: ["name","status"]
proxy
:
type
: "ajax"
url
: "status.yaws"
reader :
102 | Chapter 9: Building an Application with OTP
)
console.log(store)
store
type: "json"
root: "data"
setupMultiCast = ->
store = makeStore()
form = Ext.create("Ext.form.Panel",
buttons:
{
xtype: "button"
text: "Set Status"
handler: () ->
values = form.getValues()
console.log(values)
Ext.Ajax.request(
url: "set-status.yaws",
params: values
success: () ->
store.load()
alert("Data Reloaded")
)
)
grid
}
title: "Set Status"
items: [
{
xtype
name
fieldLabel
width
}
{
xtype
name
fieldLabel
width
}
]
: "textfield"
: "name"
: "User"
: 400
: "textarea"
: "status"
: "Status"
: 400
= Ext.create("Ext.grid.Panel",
width
: 500
height
: 350,
frame
: true
renderTo : "multi_cast"
store
: store
title
: "User Status"
bbar
: form
buttons : [
{
text: "Reload"
handler: () -> store.load()
Some Client-Side Code | 103
}]
columns:
[
{
}
{
]
}
text: "User"
width: 80
sortable: true
dataIndex: "name"
text: "Status"
dataIndex: "status"
sortable: true
width: 300
)
Ext.onReady setupMultiCast
Figure 9-3. Multicast application
Let’s Have Some Adult Supervision Around Here!
Our server will run for a long time distributing messages between various users. Sooner
or later something will go wrong. If that happens the process will terminate, and we
want to define what happens next. In this case, several things need to happen: first of
all, the server should be restarted, and we also want to log what happened so we can
fix it later.
104 | Chapter 9: Building an Application with OTP
OTP uses the concept of a supervisor to do all of these things, and thankfully building
a basic supervisor is pretty easy. The basic supervisor is the supervisor behavior. Like
a gen_server, you just need to create a module that exports a few functions, and also
like a gen_server, the Emacs Erlang mode will create a template you can use. Exam-
ple 9-8 is based on the Emacs template with some of the comments removed for space.
Example 9-8. Setting up our Supervisor
%%%-------------------------------------------------------------------
%%% @author Zach Kessin <>
%%% @copyright (C) 2012, Zach Kessin
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2012 by Zach Kessin <>
%%%-------------------------------------------------------------------
-module(multi_cast_sup).
-behaviour(supervisor).
%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).
%%%===================================================================
%%% API functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
supervisor:start_link({local, ?SERVER}, ?MODULE, []).
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%
ignore |
%%
{error, Reason}
Let’s Have Some Adult Supervision Around Here! | 105
%% @end
%%--------------------------------------------------------------------
init([]) ->
RestartStrategy
= one_for_one,
MaxRestarts
= 1000,
MaxSecondsBetweenRestarts
= 3600,
SupFlags = {RestartStrategy,
        MaxRestarts,
       MaxSecondsBetweenRestarts},
Restart = permanent,
Shutdown = 2000,
Type = worker,
AChild = {'process_id',
      {'AModule', start_link, []},
     Restart,
    Shutdown,
   Type,
  ['AModule']},
{ok, {SupFlags, [AChild]}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
When creating a supervisor, you need to create a module that exports an init/1 func-
tion. This function is called when the supervisor starts up and defines the rules for how
and when workers are created and restarted.
The supervisor is run as its own process that does nothing but monitor other processes
(these can be other supervisors or the ones doing the actual work). When these other
processes die, the supervisor will restart them; it can also kill them when it’s time to
shut down an application.
In addition, the supervisor is where worker processes are started. This is done by listing
the processes in the supervisor init/1 function as part of the return value. For each
process we get a structure like in Example 9-9. The first field is a process ID that is used
by the supervisor internally; it may show up in listings but you can probably ignore it.
The second term tells how to start our server. It should contain the module, the function
to start the module (normally start_link), and any parameters to pass to that function.
Example 9-9. Process init structure
{
Process_id,
{Module, start_link, []},
Restart,
Shutdown,
Type,
106 | Chapter 9: Building an Application with OTP
}
[Module]
The Restart term tells the supervisor how and when to restart this server if needed.
Options are permanent, temporary, and transient. A permanent process will always be
restarted. A temporary process will never be restarted. A transient process will be re-
started if it dies unexpectedly, but if it terminates normally it will not be restarted.
The supervisor includes protection against a process that is stuck restarting itself and
instantly exiting. If you set the MaxR and MaxT values it will limit the process to a set
number of restarts in a period of time. In our example we limit the number of restarts
to 1000 per hour; in a production system these numbers will need to be adjusted for
the specifics of the application.
The Shutdown term comes into play when it is time to shut down an application. When
an application is being shut down worker, processes may need to close resources or
otherwise clean up. As such, the supervisor should allow time to do that. In this case,
set Shutdown to a time in milliseconds for how long each process can take before the
supervisor will kill it outright. To kill a process with no warning set this to bru
tal_kill; if the processes may need a very long time, set this to infinity. Getting this
setting right may take some fine-tuning.
The Type parameter can have two possible values: worker or supervisor. That is, the
process can be a worker or a lower level of supervision.
Finally we have a list of modules. Normally this will be the same module as used above
unless we are doing something weird and dynamic, in which case it might be the atom
dynamic.
Now that we have our supervisor built we need to test it out. To do this we want to
make our server die due to a bug and see that it restarted. So we’ll introduce a new
clause in the update_user_status/3 function that will cause the process to crash when
a user sets a blank status message:
update_user_status(_users,_User, "") ->
ok = status;
As you can see in Example 9-10, when we set the user’s status to an empty string, we
get an error and the process terminates.
When running this from the Erlang shell, run unlink(SupervisorPid).
before testing restarts. Otherwise the crashing process will also crash
the shell (which restarts) and the supervisor.
Example 9-10. Running the supervisor
2> multi_cast_server:start_link().
{ok,<0.63.0>}
3> multi_cast_server:update_status("Zach","").
multi_cast_server:136 (<0.63.0>) Update "Zach" -> []
Let’s Have Some Adult Supervision Around Here! | 107
STATE []
=ERROR REPORT==== 3-Apr-2012::13:04:00 ===
** Generic server multi_cast_server terminating
** Last message in was {update_status,"Zach",[]}
** When Server state == []
** Reason for termination ==
** {{badmatch,status},
[{multi_cast_server,update_user_status,3},
{multi_cast_server,handle_call,3},
{gen_server,handle_msg,5},
{proc_lib,init_p_do_apply,3}]}
** exception exit: {badmatch,status}
in function multi_cast_server:update_user_status/3
in call from multi_cast_server:handle_call/3
in call from gen_server:handle_msg/5
in call from proc_lib:init_p_do_apply/3
Ideally, you want each unit of work that can crash to be in its own process so that the
smallest possible unit of work will crash. In this case we could take the function to
create the updated state and put it in its own stateless process that would simply take
a state, update it, and return it.
A Little Optimization
If you want to enhance this module for more speed, create a second module that will
cache the JSON and create a hash of the data as an ETag. Then have the browser send
a request with an HTTP IF-None-Match header, and if the data has not changed it will
just get back an HTTP 304 header and use the data that it already has. And the nice
thing is that the server will not have to compute the hash or create the JSON for each
request, but can do it only when data is changed.
For Etags to be effective they need to be consistent. If each time the user
makes a request she is shown a different server that has a different value,
then the Etag will be useless. So in order for this to work, the load bal-
ancer needs to make sure that the same user is always shown the same
server, or that there is some other mechanism for making sure that the
user sees consistent data from the ETag.
This server keeps track of two pieces of information: the output JSON, and an MD5
hash that will be used by the browser to determine if it has up-to-date data.
The crypto:md5/1 function returns an array of numbers that is the raw
hash of the data. We use the base64:encode_to_string/1 function to
turn it into a string like "4CNCRcsAqiYMz6mamgsjXg==", which looks like
something one would expect to get from MD5.
108 | Chapter 9: Building an Application with OTP
We also get an additional bit of reliability here. We can set this up so that when this
server crashes it will just restart and have the init/1 function automatically query the
server that holds the data to refresh it. On the other hand, if that server crashes we can
set it so that this server will also be restarted so there is no stale data. This is done in
Example 9-11, which creates a server to create the JSON data.
Example 9-11. Caching the results (multi_cast_front)
%%%-------------------------------------------------------------------
%%% @author Zach Kessin <>
%%% @copyright (C) 2012, Zach Kessin
%%% @doc
%%%
%%% @end
%%% Created : 5 Apr 2012 by Zach Kessin <>
%%%-------------------------------------------------------------------
-module(multi_cast_front).
-behaviour(gen_server).
%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).
-export([get_json/0, get_etag/0, update_status/2]).
-export([convert_to_json/1, make_state/0]).
-define(SERVER, ?MODULE).
-record(state, {etag, json}).
get_json() ->
gen_server:call(?MODULE, {get_json}).
get_etag() ->
gen_server:call(?MODULE, {get_etag}).
update_status(User, Status) ->
multi_cast_server:update_status(User, Status),
gen_server:call(?MODULE, {update_status}).
%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
A Little Optimization | 109
%% @end
%%--------------------------------------------------------------------
start_link() ->
gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
io:format("~n~p:~p(~p) init(~p)~n",
[?MODULE, ?LINE, self(), []]),
State = make_state(),
{ok, State}.
handle_call({get_json}, _From, State) ->
{reply, State#state.json, State};
handle_call({get_etag}, _From, State) ->
{reply, State#state.etag, State};
handle_call({update_status}, _From, _State) ->
NewState = make_state(),
{noreply, NewState}.
handle_cast(_Msg, State) ->
{noreply, State}.
handle_info(_Info, State) ->
{noreply, State}.
terminate(_Reason, _State) ->
ok.
code_change(_OldVsn, State, _Extra) ->
{ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
convert_to_json(Data) ->
Content = [{obj, [
{name,
list_to_binary(Name)},
{status, list_to_binary(Status)}]} ||
{Name, Status} <-Data],
{obj, [{data, Content}]}.
make_state () ->
{ok, Data}
= multi_cast_server:get_current_user_status(),
110 | Chapter 9: Building an Application with OTP
io:format("~n~p:~p(~p) new data ~p~n",
[?MODULE, ?LINE, self(), Data]),
Json
= rfc4627:encode(convert_to_json(Data)),
Etag
= base64:encode_to_string(crypto:md5(Json)),
io:format("~n~p:~p(~p) new data Etag: ~p ~p~n",
[?MODULE, ?LINE, self(), Etag, Json]),
NewState
= #state{
json = Json,
etag = Etag},
NewState.
We also need to change the supervisor in Example 9-8 so that it will start both servers
and restart them in the correct way. I have added a second server under the name
“Front” in addition to the initial server we had in the first example.
I have also changed the restart strategy from one_for_one to rest_for_one. This ensures
that since the frontend server is started after the main server it will be restarted if the
main one is, but not the other way around. This new supervisor is shown in Exam-
ple 9-12.
Example 9-12. Setting up our supervisor (Take 2)
-module(multi_cast_sup2).
-behaviour(supervisor).
%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).
%%%===================================================================
%%% API functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
supervisor:start_link({local, ?SERVER}, ?MODULE, []).
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
io:format("~n~p:~p (~p) init([]) ~n",
[?MODULE, ?LINE, self()]),
A Little Optimization | 111
RestartStrategy
= rest_for_one,
MaxRestarts
= 1000,
MaxSecondsBetweenRestarts
= 3600,
ServerName
= multi_cast_server,
ServerFrontName
= multi_cast_front,
SupFlags
= {RestartStrategy,
MaxRestarts,
MaxSecondsBetweenRestarts},
Restart
Shutdown
Type
= permanent,
= 2000,
= worker,
Server
= {'multi_cast_server_id',
{ServerName, start_link, []},
Restart,
Shutdown,
Type,
[ServerName]},
Front
={'multi_cast_front_id',
{ServerFrontName, start_link, []},
Restart,
Shutdown,
Type,
[ServerFrontName]},
{ok, {SupFlags, [Server, Front]}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
We will also need to change the status.yaws and set-status.yaws files (Examples 9-4
and 9-6 )to reflect the new interfaces.
The case of set-status.yaws is pretty simple, as we just need to change the call from
multi_cast_server:update_status/2 to multi_cast_front:update_status/2 (see Exam-
ple 9-13).
Example 9-13. Setting the status with the front controller (set-status2.yaws)
<erl>
out(Arg) ->
{ok, Name}
= postvar(Arg, "name"),
{ok, Status} = postvar(Arg, "status"),
io:format("~n(~p) Name ~p, Status ~p ~n",
[self(), Name, Status]),
multi_cast_front:update_status(Name, Status),
{html, "true"}.
</erl>
112 | Chapter 9: Building an Application with OTP
However, we have a bit of extra work to do in status2.yaws. Here we no longer have
to convert the data to JSON ourselves, but we do have to check for a cache hit. If the
browser sends an If-None-Match header, we will get the value of that header and com-
pare it to the ETag that has been stored on the server. If they are the same, we should
send back the 304 status code and tell the browser to use its cached copy of the data;
otherwise we send back the actual data, and of course set an ETag in the header.
Example 9-14. Getting status with ETags (status2.yaws)
<erl>
get_etag_header(Arg) ->
Headers = Arg#arg.headers,
Headers#headers.if_none_match.
get_response(Current_Etag, Header_Etag)
when Current_Etag =:= Header_Etag ->
{status, 304};
get_response(Current_Etag, _Header_Etag) ->
JSON = multi_cast_front:get_json(),
io:format("~n (~p) JSON -> ~p~n", [self(), JSON]),
[
{content, "application/json", JSON},
{header, "Etag: "++ Current_Etag}
].
out(Arg) ->
Header_Etag
= get_etag_header(Arg),
Current_Etag
= multi_cast_front:get_etag(),
io:format("~n (~p) If-None-Match: ~p ~n", [self(), Header_Etag]),
io:format("~n (~p) ETag: ~p ~n", [self(), Current_Etag]),
get_response(Current_Etag, Header_Etag).
</erl>
When the code in Example 9-14 is run, the response will look like Example 9-15 if the
If-None-Match header is not set or if it does not match.
Example 9-15. The output from Example 9-14
HTTP/1.1 200 OK
Server: Yaws/1.92 Yet Another Web Server
Date: Tue, 10 Apr 2012 15:44:58 GMT
Content-Length: 12
Content-Type: application/json
Etag: 4CNCRcsAqiYMz6mamgsjXg==
{"data":[]}
If the header does match then the system will return Example 9-16, which will let the
client know that the data has not changed.
A Little Optimization | 113
Example 9-16. Cache hit from Example 9-14
HTTP/1.1 304 Not Modified
Server: Yaws/1.92 Yet Another Web Server
Date: Tue, 10 Apr 2012 15:49:30 GMT
Content-Length: 1
Content-Type: text/html
Bundling as an Application
When we take the supervisors (“Let’s Have Some Adult Supervision Around
Here!” on page 104) and the actual workers, we can package them together into an
“application” that can be started and stopped in a standard way by Erlang. Once you
do this, your application servers can be started by calling application:start/1,2. Pass
it the name of the application to start and, if needed, an array containing any parameters
that should be passed when starting the application.
The application behavior can be in the same module as the supervisor, as they share
no function names, but it is probably better to leave them separate. In general the
application should be named APPNAME_app.erl and the supervisor APPNAME_sup.erl.
Erlang uses the term “application” to refer to a related set of services.
Not to be confused with the more conventional use of the term.
To create an application, we use the application template from Emacs. The application
behavior has two functions called start/2 and stop/1. The start function will be called
when the application is started, and the stop function is called when the application is
stopped. Normally these will be used for setup and cleanup.
Example 9-17. Setting up our application
%%%-------------------------------------------------------------------
%%% @author Zach Kessin <>
%%% @copyright (C) 2012, Zach Kessin
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2012 by Zach Kessin <>
%%%-------------------------------------------------------------------
-module(multi_cast_app).
-behaviour(application).
%% Application callbacks
-export([start/2, stop/1]).
%%%===================================================================
%%% Application callbacks
114 | Chapter 9: Building an Application with OTP
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%
{ok, Pid, State} |
%%
{error, Reason}
%%
StartType = normal | {takeover, Node} | {failover, Node}
%%
StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
io:format("~n~p:~p (~p) start(~p, ~p) ~n",
[?MODULE, ?LINE, self(), _StartType, _StartArgs]),
case multi_cast_sup2:start_link() of
{ok, Pid} ->
{ok, Pid};
Error ->
Error
end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
The App File
Every application in Erlang has an application file that lives in the ebin directory. This
file will consist of a big data structure that looks like Example 9-18. The file should
normally be titled something like my_app_name.app—in our case multi_cast.app.
The App File | 115
This file has a number of fields, and in practice when creating one you will want to pull
up the documentation as to what each field means, or base a new file on an older
example.
The tuple starts with the atom application, followed by an atom that is the name of
the application. After that is a large property list with a few key fields.
The description field is a description of the application. The vsn field is the version
number of the application. The next field, modules, will contain a list of the modules
that an application uses. OTP will ensure that a module belongs to only one application
and that all of these modules are present. Some of these applications will be server
processes and others can be just groups of functions. Listing those modules here will
ensure that the system will let you know if they are absent.
The registered field contains a list of the names registered by an application. This is
useful because if two applications try to register the same name, OTP will know that
and issue a warning.
The final item is mod, which is the callback module with the application behavior. If
your app does not have any services that need to be started (for example if it’s just a
bunch of pure functions), omit this line.
The Example 9-18 file must have a period (“.”) at the end or it won’t
work, and will give a really cryptic-looking error message.
Example 9-18. Setting up our application (multi_cast.app)
{
application,
multi_cast,
[
{description, "Multi Cast Example"},
{vsn, "1.0.0"},
{modules,
[
multi_cast_app,
multi_cast_sup2,
multi_cast_server,
multi_cast_front
]},
{registered, [multi_cast_server, multi_cast_front, multi_cast_sup2]},
{mod, {multi_cast_app, []}}
]
}.
Once our applications have been defined and tested from the command line, we can
set up Erlang to automatically start our application when it starts. This can be done by
adding the like application:start(MyApplication). to the .erlang file or by passing a
flag to the Erlang VM on starting.
116 | Chapter 9: Building an Application with OTP
The application framework also provides us a clean way to start, stop, and upgrade our
application with all its dependencies.
Wrapping Up OTP
This chapter is not a full introduction to OTP, nor was it intended to be. However, I
wanted to introduce some of the basic structure of OTP as a way to show how to
structure an application.
The application we built in this chapter has some inherent limits; for one thing, the
user’s status is stored in a list, which makes updates an O(N) operation. Since I intended
to use this where the number of items in the list is small, fewer than 25 or so, it should
not be a major problem. It would of course be possible to change the data structure to
use a different data structure if needed.
In addition, this application assumes that only one copy of the servers will be running
on a node. A more realistic goal would be to have a master supervisor that would create
a supervisor and servers for each group of users. In such a case we would have a large
number of groups of users, each with their own servers and supervisor. Above those
would be one master supervisor that would restart the local supervisor if needed, which
could then restart the servers.
In short, there are many things that were not covered here. I did not cover gen_fsm or
gen_event, but these can provide other types of servers to nicely complement the
gen_server.
Wrapping Up OTP | 117
APPENDIX A
Installing Erlang and Yaws
Erlang can be installed from http://www.erlang.org. There are versions for Mac, Linux,
and Windows. In addition, most popular Linux distributions have packages for Erlang.
As of this writing, the current version is R15B01.
Many modern Linux distributions will feature Erlang and Yaws in their basic package
system. So from Ubuntu, doing a simple sudo apt-get install yaws will install Erlang
and Yaws (and any dependencies). Similarly, Fedora Linux includes both Erlang and
Yaws as part of the basic packages, which can be installed via yum.
Yaws can be found at http://yaws.hyber.org and downloaded from there. Once again,
most Linux distributions will include packages. As of January 2012, the current version
is 1.92.
In Ubuntu Linux, doing a simple apt-get install yaws will install Yaws with all the
dependencies, including Erlang itself.
For Microsoft Windows, there are installers for Yaws that can be downloaded from
http://yaws.hyber.org. However, before Yaws can run, Erlang itself must be downloaded
separately.
119
APPENDIX B
Beyond Yaws
This book has been an introduction to using Yaws with Erlang. However, there are a
number of other web servers and frameworks for Erlang that may make more sense for
a specific project.
Web Servers
In addition to Yaws, there are two other Erlang web servers in active development:
Cowboy and MochiWeb. Each has a different set of pros and cons that should be
considered.
A detailed comparison of different Erlang web server options could be a book in and
of itself. However, a good place to start is http://www.ostinelli.net/a-comparison-be
tween-misultin-mochiweb-cowboy-nodejs-and-tornadoweb/, which attempts to com-
pare Misultin, MochiWeb, and Cowboy along with Node.js (JavaScript) and Torna-
doweb (Python).
Cowboy
Cowboy (https://github.com/extend/cowboy) is a new web server for Erlang designed to
be small, fast, and modular. It is also designed to be easy to embed in other applications,
which can be useful if you are creating a web interface to a larger Erlang application.
To set up a Cowboy server you must create some socket listeners and handlers to deal
with incoming requests. So compared to Yaws, there is a bit more upfront set up re-
quired, you can’t just give it a bunch of static files or embed Erlang in .yaws files and
go. You must explicitly tell it how to handle requests.
Cowboy can also handle web sockets, and there is example code on the Cowboy Git-
Hub page; however, work is still ongoing on that project. It is unclear how much sup-
port there is for file uploads, streaming data, and the like.
121
MochiWeb
MochiWeb (https://github.com/mochi/mochiweb) is not so much a web server as a set
of components for building web servers. As such, it does not include many things that
are included with Yaws, such as a URL dispatcher; you will have to create that on your
own.
That being said, MochiWeb has a dedicated following on the Web, and many of the
web frameworks use it as a base. You can find a MochiWeb tutorial at https://github
.com/mochi/mochiweb.
Misultin
Misultin is an Erlang web server that has unfortunately been discontinued. The devel-
opers felt that with Cowboy, MochiWeb, and others running around that supporting
yet another Erlang web server was too much duplicated effort. As such, they are sup-
porting existing users but suggesting that everyone move to Cowboy (“Cow-
boy” on page 121) or MochiWeb (“MochiWeb” on page 122). Several of the frame-
works for Erlang can run on Misultin, but most can also run on MochiWeb or Yaws.
Web Frameworks
There are currently at least six Erlang web frameworks that can be used: BeepBeep,
Chicago Boss, Erlang Web, ErlyWeb, Nitrogen, and Zotonic. The Chicago Boss team
maintains a high-level grid showing what frameworks support which features that can
be found here: https://github.com/evanmiller/ChicagoBoss/wiki/Comparison-of-Erlang
-Web-Frameworks.
BeepBeep, Erlang Web, and ErlyWeb seem to have not been updated
since 2008 or 2009, so I am not going to cover them further.
Chicago Boss
Chicago Boss (http://www.chicagoboss.org) is a full-featured web framework originally
built around Misultin and currently moving to Cowboy. It is under active development,
has a nice website with a great tutorial, and has a lot of flexibility.
Chicago Boss is built around an MVC architecture, which should feel familiar to pro-
grammers who have used other MVC frameworks like Symfony on PHP or Ruby on
Rails. It also features a built-in Queue system called “BossMQ” that can be used to link
pieces of a larger application. Using BossMQ is as simple as calling boss_mq:push/2 to
add a message to a queue and boss_mq:pull/2 to get a message from the queue. (For
122 | Appendix B: Beyond Yaws
more details, see the Chicago Boss website.) This can be used with long polling, but
for now it does not appear that Chicago Boss supports web sockets.
In terms of data storage, Chicago Boss features a lot of options. You can store your data
in Mnesia, MongoDB, MySQL, PostgreSQL, Riak, or Tokyo Tyrant. It also uses Er-
lyDTL (“ErlyDTL” on page 26) for templates.
In general, Chicago Boss seems to be very well thought out and has a solid website with
well-written documentation.
Nitrogen
Nitrogen (http://nitrogenproject.com/) is an event-based framework created by Rusty
Klophaus. It uses its own template system instead of ErlyDTL or the like. It also seems
to want to generate HTML in Erlang, while many modern systems will want to send
JSON data to the browser and have the “View” layer running in a browser.
Zotonic
Zotonic (http://zotonic.com) is a CMS and framework for Erlang, and if you have used
Drupal, it will probably feel pretty familiar. It features a rich set of management screens
to allow a nondeveloper to manage a Zotonic website. It advertises a lot of features out
of the box and is under active development. If you want to present web content with
an Erlang backend, Zotonic may be a great choice!
Zotonic is built on MochiWeb and uses Postgres to store its data.
Web Frameworks | 123
APPENDIX C
Interfacing with Ruby and Python
So you’ve been reading this book and thinking that Erlang sounds amazingly cool. You
already have a web infrastructure in PHP, Python, or Ruby but you would love to be
able to build some structure in Erlang. There are several ways you can do this. You
could use something like RabbitMQ to couple different parts of an application with
queues. You could also create a web service in Erlang and access it from another lan-
guage, or send data over a socket. However, what would be really nice is being able to
have native communications between Erlang and some other language.
This is actually fairly simple to do. As we know by now, Erlang processes can commu-
nicate over a wire protocol. When you send a message from process A to process B, all
the data is serialized in some way and sent across to the other process to receive.
Knowing that, you might think it would be pretty easy to create a package in another
language that can speak that protocol and work with it, and of course you would be
right. There are packages for Ruby and Python (and others) that can do that quite well.
(Sending Erlang functions to other languages probably won’t work.)
These interfaces can be used to mate a frontend in Python or Ruby to an Erlang backend
or vice versa. They can also be used when porting some code to Erlang for testing. If
you have a module that has been well tested in Ruby, for example, you could use
QuickCheck to try a large number of test cases on both the Ruby and Erlang versions
and make sure that both work correctly.
In order to have an Erlang node in some other language, there are a few actions that a
package must be able to perform. It must be able to connect to an existing node and
disconnect when done, and of course it must be able to send and receive messages from
other nodes.
There is a module to interface PHP with Erlang called “mypeb” (http://
mypeb.googlecode.com). However, I was unable to get it to compile.
125
Ruby
Erlang and Ruby can be interfaced with the erlectricity package that can be found at
http://code.google.com/p/erlectricity/source/browse/. This package provides a Ruby in-
terface that can interact with Erlang.
In Example C-1, which was taken from the erlectricity examples, after some initial
setup there is a receive block created in Ruby. Here the syntax is distinctively Ruby,
but the semantics directly parallel that of a receive block in Erlang.
The semantics of the receive block match those in Erlang. Here receive opens a block.
The receive block uses the method f.when (line 8) to parallel the structure in Erlang
quite nicely. Also note that the recursive structure of an Erlang loop is reproduced with
the g.receive_loop method call.
To send a message from Ruby to Erlang you need the process ID. You would then use
f.send! :result, graph.to_blob, which will send the message to the Erlang process.
The Erlang process will receive a tuple of the form {result, Blob}.
Example C-1. gruff_provider.rb
$:.unshift(File.dirname(__FILE__) + "/../../lib/")
require 'erlectricity'
require 'rubygems'
require 'gruff'
receive do |f|
f.when(:plot, String, Symbol, String) do |name, style, font|
graph = Gruff.const_get(style).new
graph.title = name
graph.font = font
graph.legend_font_size = 10
f.receive do |g|
g.when(:data, Symbol, Array) do |name, points|
graph.data name, points
g.receive_loop
end
g.when(:labels, Erl.hash) do |label_data|
graph.labels = label_data
g.receive_loop
end
g.when(:end){ :ok }
end
f.send! :result, graph.to_blob
f.receive_loop
end
end
126 | Appendix C: Interfacing with Ruby and Python
Python
You can interface Python with Erlang using py_interface (http://www.lysator.liu.se/
~tab/erlang/py_interface/), which is a Python implementation of an Erlang node.1 I
checked the code out of Git and ran the script howto-make-a-new-version followed by
configure and make, all of which seemed to work. My system is Ubuntu 11.10 and has
Python version 2.7.2+ on it.
In theory, you could create one node in Python and another in Ruby,
and have them send messages back and forth with no Erlang in between.
However, I am not sure this is actually a useful thing to do or a good idea.
While py_interface does not have a great manual, it does have a decent readme file
and a good set of examples that can be used as a basis to explore how to do things. The
bad thing is that the syntax of this module is not nearly as nice as that seen in Ruby.
Python is not really concurrent in the way that Erlang is, so the way things work is a
bit different. Specifically, the Python API is single threaded and uses callbacks to handle
messages from Erlang. The module also tries to map Erlang types onto Python as much
as possible, and uses classes when it can’t.
You can see a basic example of how to set up a Python node in Example C-2, which is
taken from the py_interface examples. It imports erl_node, erl_opts, and erl_even
thandler from the py_interface module. Then in the main() function it sets up a node
with
the
function
erl_node.ErlNode(ownNodeName,
erl_opts.ErlNo
deOpts(cookie=cookie)). It then publishes the node so that other processes can find it
and registers the mailbox with n.CreateMBox(__TestMBoxCallback). The callback
__TestMBoxCallback is what actually responds to any incoming messages from Erlang.
The rest of the function just sets up an event loop to wait for requests to come in.
Example C-2. Python example
#! /usr/bin/env python
import sys
import getopt
from py_interface import erl_node
from py_interface import erl_opts
from py_interface import erl_eventhandler
###
###
###
### TEST CODE
1. py_interface is licensed under the LGPL.
Python | 127
###
###
def __TestMBoxCallback(msg):
print "msg=%s" % `msg`
n=None
m=None
def main(argv):
try:
opts, args = getopt.getopt(argv[1:], "?n:c:")
except getopt.error, info:
print info
sys.exit(1)
hostName
= "localhost"
ownNodeName = "py_interface_test"
cookie
= "cookie"
for (optchar, optarg) in opts:
if optchar == "-?":
print "Usage: %s erlnode" % argv[0]
sys.exit(1)
elif optchar == "-c":
cookie = optarg
elif optchar == "-n":
ownNodeName = optarg
print "Creating node..."
n = erl_node.ErlNode(ownNodeName, erl_opts.ErlNodeOpts(cookie=cookie))
print "Publishing node..."
n.Publish()
print "Creating mbox..."
m = n.CreateMBox(__TestMBoxCallback)
print "Registering mbox as p..."
m.RegisterName("p")
print "Looping..."
evhand = erl_eventhandler.GetEventHandler()
evhand.Loop()
main(sys.argv)
128 | Appendix C: Interfacing with Ruby and Python
APPENDIX D
Using Erlang with Emacs
For many Erlang developers, the editor of choice is Emacs. As a longtime Emacs user
I have found there to be many reasons for this.
One feature of the Emacs Erlang mode is that you can compile a file by typing “C-c C-
k”, which will open Erlang in a shell and compile the buffer you are in. You can also
copy and paste code from a buffer to an Erlang shell.
Being able to copy and paste code from a buffer to the Erlang REPL makes it very easy
to explore solutions to a problem. The programmer can create a simple module and
load it into Erlang with “C-c C-k”, then create a bunch of test cases that are copied via
standard Emacs editing to the shell to ensure that the code is working correctly. This
does not replace formal testing, but supplements it when doing development.
In addition, the Erlang mode has a set of templates that can be used to create common
structures. So if you need to work with the OTP gen_server pattern, you can generate
the skeleton of that structure by opening up a new buffer and selecting the correct
structure. The template for gen_server is shown in Example D-3 at the end of this
chapter.
Erlang comes with a very nice Erlang mode. You can find more details
on it at http://www.erlang.org/doc/apps/tools/erlang_mode_chapter
.html.
Distel
If you want to have a powerful interface between Emacs and Erlang, check out Distel
mode. Distel (short for Distributed Emacs Lisp) extends Emacs Lisp to be able to speak
to an Erlang node in a way very similar to Ruby or Python, as shown in Appendix C.
The Distel package can be found at http://fresh.homeunix.net/~luke/distel/ and down-
loaded from there.
129
When starting up Distel, you need to tell it which node to talk to. It will prompt you
for a node name, which can be in the form node@host or just node if it is local. Once you
have given Distel a node name it will continue to use that node unless you prefix a
command with C-u node-name.
Distel supports a number of features to make coding Erlang in Emacs easier. First of
all, it supports completions of modules and functions by hitting M-?. It also allows you
to load and evaluate Erlang code from the minibuffer. To load a module, use C-c C-d
L and Distel will prompt you for the module name. Distel also features some pretty
fancy refactoring tools.
In addition to tools, Distel also features some applications that can make working with
Erlang easier. It features a process manager that will work from an Emacs buffer (C-c
C-d l) and that allows you to find out all sorts of information about a process, including
a backtrace and the contents of the process mailbox.
Distel will also allow you to interface Erlang’s debugging facilities with Emacs, and will
let you do all sorts of things to debug and profile Erlang code as it runs.
Finally, Distel will let you have a more powerful interactive session then the standard
Erlang command line. Based on the Emacs Lisp *scratch* buffer, running an interactive
session allows you to evaluate code directly from an editor buffer and see the results.
The Distel manual is only 11 pages long and can be found on the Distel web page. It’s
worth reading.
Flymake Mode
If you like using an IDE that highlights errors as you type, check out Flymake mode,
which provides exactly that feature in Emacs. Flymake mode supports many languages,
including Erlang.
In order to use Flymake, add the Emacs Lisp code in flymake.el (Example D-1) to
your .emacs file. You may need to change the path to “flymake_emacs” to match the
location of the file on your system.
Example D-1. flymake.el
(require 'flymake)
(defun flymake-erlang-init ()
(let* ((temp-file (flymake-init-create-temp-buffer-copy
'flymake-create-temp-inplace))
(local-file (file-relative-name temp-file
(file-name-directory buffer-file-name))))
(list "~/bin/flymake_erlang" (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks
'("\\.erl\\'" flymake-erlang-init))
130 | Appendix D: Using Erlang with Emacs
The Emacs Lisp calls the binary “flymake_erlang” to compile code as you type (Ex-
ample D-2). This uses escript, which is a tool to write scripts in Erlang as you might
do in Perl or Python. It does this by calling the function main/1 with a list of parameters.
Example D-2. flymake_erlang
#!/usr/bin/env escript
-export([main/1]).
main([File_Name]) ->
compile:file(File_Name, [warn_obsolete_guard, warn_unused_import,
warn_shadow_vars, warn_export_vars,
strong_validation, report,
{i, "../include"}]).
Gen Server Template
Example D-3. gen_server.erl
%%%-------------------------------------------------------------------
%%% @author Zach Kessin <zkessin@gmail.com>
%%% @copyright (C) 2012, Zach Kessin
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2012 by Zach Kessin <zkessin@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_server).
-behaviour(gen_server).
%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-record(state, {}).
%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
Gen Server Template | 131
start_link() ->
gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%
{ok, State, Timeout} |
%%
ignore |
%%
{stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
{ok, #state{}}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%
{reply, Reply, State} |
%%
{reply, Reply, State, Timeout} |
%%
{noreply, State} |
%%
{noreply, State, Timeout} |
%%
{stop, Reason, Reply, State} |
%%
{stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
Reply = ok,
{reply, Reply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%
{noreply, State, Timeout} |
%%
{stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
{noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
132 | Appendix D: Using Erlang with Emacs
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%
{noreply, State, Timeout} |
%%
{stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
{noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
ok.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
{ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
Gen Server Template | 133
About the Author
Zachary Kessin has been working on developing interactive web applications since
1994. In the last few years, Zachary’s focus has been on building complex applications
in the browser with JavaScript, browser-based testing with Selenium, functional pro-
gramming, and code generation.

