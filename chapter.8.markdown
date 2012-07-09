CHAPTER 8
Using the HTTP Client
Sometimes you need not to create an HTTP service but to consume one—perhaps to
use a RESTful service or other API, or to load-test a server by hitting it with lots of
requests at once. Erlang provides an HTTP client API that lets you do this. You can
find the manual page for this module at http://www.erlang.org/doc/man/httpc.html. The
httpc module is part of the standard Erlang distribution and does not have to be in-
stalled separately.
The first thing to do before using the httpc module is to start the inets service by calling
inets:start() or application:start(inets). If you are running an application this can
be done from the .erlang file or from the command line in testing. If you do not start
inets, httpc will not work correctly. 1
If there are some options that must be set for all calls then you can use the function
httpc:set_options/1 or httpc:set_options/2. There are a number of options that can
be set here, including all of the standard ones you would expect. Of particular note is
the max_sessions option, which defaults to 2. In addition, if you need to set a proxy
server you can do it here with the Proxy option. When calling set_option it will return
ok or {error, Reason}.
There are several other HTTP client packages for Erlang that provide
more features, including ibrowse and lhttpc. You can find both online.
Making a Request
There are four functions available to make an HTTP request that run from one to five
parameters (there is no function with three). These provide progressively more control
over the HTTP request.
1. The inets service is a part of the standard Erlang distribution.
83
The simplest version of an HTTP request is httpc:request/1, which takes a URL as an
argument. The request/1 function simply performs an HTTP GET operation on the
supplied URL as shown in Example 8-1. In this case a request to http://www.goo
gle.com returns {ok, Response} or {error, Reason}. The Response will be the headers
of the HTTP request along with the body of the request (truncated in Example 8-1).2
If you wish to extract the values from a successful request you can use this line to extract
the variables: {ok, {{Version, 200, ReasonPhrase}, Headers, Body}}.
Example 8-1. A simple HTTP request
14> httpc:request("http://www.google.com").
{ok,{{"HTTP/1.1",200,"OK"},
[{"cache-control","private, max-age=0"},
{"date","Tue, 24 Apr 2012 17:59:10 GMT"},
{"server","gws"},
{"content-length","40887"},
{"content-type","text/html; charset=windows-1255"},
{"expires","-1"},
{"x-xss-protection","1; mode=block"},
{"x-frame-options","SAMEORIGIN"}],
"<!doctype html>..."}}
Sometimes a simple GET is not enough control—for example, you wish to access a
REST service where you may need to send data with POST, PUT, or DELETE, or to
test for the existence of a resource with a HEAD request.
To have more control over the request, use request/4 or request/5. The first parameter
here will be the HTTP verb set as an atom. Next will be the content of the request,
followed by HTTP options, general options, and finally a profile parameter. (For a full
list of options, see the manual page.)
To post data to a service, use the request/4 version of the function as shown in Exam-
ple 8-2. In this case we are sending a simple payload of data to the server, which can
be URL-encoded or a JSON. In this example the payload is the data to be sent to the
server and the URL is the address of the resource to send it to.
Example 8-2. HTTP post
-module(post).
-export([post/3]).
post(url_encoding, URL, Payload) ->
httpc:request(post, {URL,
[],
"application/x-www-form-urlencoded",
Payload},
[],
[]);
post(json, URL, Payload) ->
2. I also removed several long lines here to make this example more readable.
84 | Chapter 8: Using the HTTP Client
httpc:request(post, {URL,
[],
"application/json",
Payload},
[],
[]).
If you do not want to have your process wait for the HTTP request for some reason,
you could wrap the request in a fun and use spawn/1 to run it in its own process. How-
ever, the http:request/4 function will do this for you if it is passed [{sync, false}]
as an option. In this case the request will return immediately and you will get the content
in a receive block. The process will be sent the message {http, {RequestId,
Result}}. This would be especially useful in the case where a program has to poll several
servers for some information and collate the results. If you are used to doing Ajax in
JavaScript this will feel familiar.
-module('async_request').
-export([async_request/1]).
async_request(URL) ->
{ok, RequestId} =
httpc:request(get, {URL, []}, [], [{sync, false}]),
receive
{http, {RequestId, Result}} ->
Result
after 500 ->
error
end.
Finally, if the HTTP request will return a large amount of data, it may be useful to have
it written to disk for further processing. To do this you can use the option {stream,
Filename} as in Example 8-3. In this case, the request/4 function will return {ok,
saved_to_file} or {error, Reason} depending on what happened. It is also possible to
stream data to a process by passing self or {self, once} instead of a filename. For
more details on how that works, look at the httpc man page on http://erlang.org.
Example 8-3. Save to a file (stream_to.erl)
-module('stream_to').
-export([stream_to/2]).
stream_to(URL, Filename) ->
httpc:request(get,
{URL,[]},
[],
[{stream, Filename}]
).
Making a Request | 85
Using OAuth
Many websites now use OAuth to provide identity services. OAuth is a protocol that
allows a user to authenticate from an external resource, such as Google or Facebook.
To use OAuth, a program just needs to know the token from the server and httpc or
another web client.
The way OAuth works is that your site redirects the user to a web page provided by
the OAuth provider, and this site then prompts the user to approve your site’s use of
OAuth. Assuming the user authorizes the access, the user will be redirected back to
your site with a token. If you then make an HTTP request to the providing site with
that token, it will respond with a JSON that provides user information, including their
name. See Example 8-4.
Example 8-4. Using OAuth (oauth.erl)
-module(oauth).
-export([auth/2]).
auth(Site, OAuthToken) ->
URL = lists:flatten(io_lib:format("~s~s", [Site, OAuthToken])),
io:format("~n~p:~p (~p)~n OAuth URL ~p~n", [?MODULE, ?LINE, self(), URL]),
{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(URL),
{ok, JSON,_} = rfc4627:decode(Body),
io:format("~n~p:~p (~p)~n JSON: ~p~n", [?MODULE, ?LINE, self(), JSON]),
JSON.
In the case of the Facebook Canvas (see the next section), once the user has authorized,
then when Facebook loads your Canvas page it will send a POST to the page with a
JSON containing an OAuth token. (Full details are on Facebook’s developer site.) Once
you have that token, you can do an https request to the Facebook server, which will
return a JSON like in Example 8-9. (Note that this JSON has been reformatted with
JSON Lint and personal information has been removed.)
Facebook Canvas
If you are building a Facebook application, one way you can interact with Facebook is
via the Canvas. When an application is opened by Facebook as a Canvas, it is opened
in an iframe inside a page from Facebook. Inside that page your app can communicate
with your server and do anything else you want it to do. You can also communicate
with Facebook via their interfaces.
When Facebook opens a Canvas page, it sends you a POST containing a signed request
that is a base64-encoded JSON allowing you to authenticate the user.
86 | Chapter 8: Using the HTTP Client
To use this data, get the signed_request field of the post data from Facebook and split
it on the period. The first part is a signature that you use your secret key to validate,
and the second part is the data to allow you to authenticate the user.
If the user has not authorized with your application, you will get a JSON like in Ex-
ample 8-5. In this case you should redirect the user to the Facebook authentication
dialog. (See Facebook’s documentation for the details: https://developers.facebook.com/
docs/authentication/canvas/.)
At this point you need to redirect the user to the Facebook authorization page.
Example 8-5. Initial JSON
{
}
"algorithm": "HMAC-SHA256",
"issued_at": 1335672795,
"user": {
"country": "il",
"locale": "en_US",
"age": {
"min": 21
}
}
To implement this, use a page like Example 8-6, which generates a basic HTML page
and then calls the code in Example 8-7 to unpack the request and send the JSON to
any included JavaScript. (You probably also want to save the data in a session cookie.)
Example 8-6. Facebook interface Yaws file (facebook.yaws)
<!DOCTYPE html>
<html>
<head>
<meta
http-equiv
="Content-Type"
content
="text/html; charset=UTF-8">
<title>Canvas</title>
</head>
<body>
<pre>
<erl>
out(Arg) ->
{ok, SignedRequest} = postvar(Arg, "signed_request"),
ParsedRequest
= facebook:parse_signed_request(SignedRequest),
facebook:response(facebook:user_has_authorized(ParsedRequest)).
</erl>
</pre>
</body>
</html>
Using OAuth | 87
The code in Example 8-7 will implement the basics of a Facebook interface. It can
decode the request from Facebook and interact with the OAuth servers.
Example 8-7. Facebook interface (facebook.erl)
-module(facebook).
-export([parse_signed_request/1,
user_has_authorized/1,
make_redirect_script/0,
get_user_id/1,
get_user_info/1,
response/1]).
-define(SECRET,
-define(APP_ID,
-define(APP_NAMESPACE,
"********************************").
"***************").
"*************").
parse_signed_request(SignedRequest) ->
[_EncodingSig, Payload]
= string:tokens(SignedRequest, "."),
PayloadJson
= tt:fb_decode_base64(Payload),
{ok, JSON, _}
= rfc4627:decode(PayloadJson),
JSON.
user_has_authorized(ParsedRequest) ->
rfc4627:get_field(ParsedRequest, "oauth_token", undefined).
get_user_id(ParsedRequest) ->
rfc4627:get_field(ParsedRequest, "user_id", undefined).
make_user_redirect_url()->
URLPatern
=
"https://www.facebook.com/dialog/oauth/?client_id=~s&redirect_uri=~s&scope=~s",
RedirectURL
= lists:flatten(io_lib:format( "https://apps.facebook.com/~s",
[?APP_NAMESPACE])),
Permission_Names
= string:join(["user_interests",
"user_location",
"user_photos",
"user_hometown",
"email"],
","),
URL
= io_lib:format(URLPatern,
[?APP_ID,
yaws_api:url_encode(RedirectURL),
Permission_Names]),
lists:flatten(URL).
make_redirect_script() ->
Url
= make_user_redirect_url(),
Tag
= "<a href=~p>~p</a>",
Script
= io_lib:format(Tag, [Url,Url]),
lists:flatten(Script).
88 | Chapter 8: Using the HTTP Client
get_user_info(OAuthToken) ->
URL = lists:flatten("https://graph.facebook.com/me?access_token="
++ binary:bin_to_list(OAuthToken)),
io:format("~n~p:~p (~p)~n OAuth URL ~p~n", [?MODULE, ?LINE, self(), URL]),
{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(URL),
{ok, JSON,_} = rfc4627:decode(Body),
io:format("~n~p:~p (~p)~n JSON: ~p~n", [?MODULE, ?LINE, self(), Body]),
JSON.
response(undefined)->
{html, facebook:make_redirect_script()};
response(OAuthToken) ->
UserInfo
= get_user_info(OAuthToken),
io:format("~n~p:~p (~p)~n JSON: ~p~n", [?MODULE, ?LINE, self(), UserInfo]),
JSON
= rfc4627:encode(UserInfo),
[
{ehtml, {script,[], "user_info_data = " ++ JSON}}].
Once the user has told Facebook that he wishes to allow your app to know who he is,
Facebook will open your page with a JSON that looks like Example 8-8. Here you will
note that there are two new fields. The first is the oauth_token that enables you to
request the user’s details from Facebook; the second is the user_id that can be used to
track sessions and locally cache user information.
Example 8-8. Authorized JSON
{
}
"algorithm": "HMAC-SHA256",
"expires": 1335679200,
"issued_at": 1335673105,
"oauth_token": "AAAB9elehJ9...",
"user": {
"country": "il",
"locale": "en_US",
"age": {
"min": 21
}
},
"user_id": "100************"
See Example 8-9 for the data that Facebook sends back from an OAuth request if the
user has allowed the app to authenticate. (Not that the JSON here has been reformatted
to make it more readable.)
Example 8-9. Using OAuth (oauth.json)
{
"id": "***************",
"name": "Joshua Levi",
"first_name": "Joshua",
"last_name": "Levi",
Using OAuth | 89
}
"link": "http:\\/\\/www.facebook.com\\/profile.php?id=***************",
"gender": "male",
"email": "zkessin\\u0040**********.***",
"timezone": 3,
"locale": "en_US",
"updated_time": "2010-10-17T10:49:04+0000"
90 | Chapter 8: Using the HTTP Client

