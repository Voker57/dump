-module(dumper).
-export([out/1,make_tables/0,crashmsg/3, toMaxUnit/1, reaper/1]).
-export([toucher/1, reap/3, get_file_info/3, get_file/3, gsub/3, start/1, config/1, gentle_toucher/1]).
-export([get_paste_languages/0, get_token_by_id/1, get_file_by_token_id/1, test/0, run_command/2, cleaner/1, soft_cleaner/0]).
-include("/usr/lib/yaws/include/yaws_api.hrl").
-include("/usr/lib/yaws/include/yaws.hrl").
-include_lib("kernel/include/file.hrl").
-include("/usr/include/erlang/s3.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include("dumper.hrl").

% Запустить вы это не сможете. Тут другой подход нужен.

% Желаю разработчикам Erlang гореть в неюникодном аду

crashmsg(_A,_B,C) ->
	{ok, F} = file:open("error.log", [append]),
	io:put_chars(F, io_lib:format("~ts\n\n", [C])),
	file:close(F),
	template([{h1, [], "INTERNAL ERROR GODDAMMIT"}, "Error have occured. Please notify ", {a, [{href, "mailto:voker57@gmail.com"}], "voker57@gmail.com"}, " about it if you want it fixed."]).

toMaxUnit(I) -> if 
	I / 1.0e9 >= 1 -> io_lib:format("~.2f GB", [I / 1.0e9]);
	I / 1.0e6 >= 1 -> io_lib:format("~.2f MB", [I / 1.0e6]);
	I / 1.0e3 >= 1 -> io_lib:format("~.2f KB", [I / 1.0e3]);
	true           -> io_lib:format("~p B", [I])
end.

start(_) ->
	crypto:start(),
	ibrowse:start(),
	{A,B,C} = now(),
	random:seed(A,B,C).

config(Key) ->
	{ok, Config} = file:consult("dumper.conf"),
	{_Key, Value} = lists:keyfind(Key, 1, Config),
	Value.

out(A) ->
	Path = case A#arg.pathinfo of
		undefined -> [];
		Str -> string:tokens(Str, "/")
	end,
	case Path of
		["upload-file"] ->
%  				{ehtml, "Disaster, a total fucking disaster."};
		case yaws_multipart:read_multipart_form(A, []) of
			{done, Params} ->
				case dict:find("file",Params) of
					{ok, [{"filename", Fname}, {temp_file, Fpath} | _]} ->
						case handle_upload(utl(Fname), utl(Fpath)) of
						{ok, Dir, Name} ->
						case dict:find("token", Params) of
								{ok, TokenId} ->
									case get_token_by_id(TokenId) of
										{ok, Token} ->
											if Token#token.status == {nothing} ->
												TheToken = Token#token{status={ok, {"files", Dir, Name}}},
												tr(fun() -> mnesia:write(TheToken) end);
												true -> ok
											end;
										_ -> ok
									end;
								_ -> ok
							end,
						case A#arg.querydata of
							"simple" ->
						{redirect_local, string:join(["","files",Dir, urlencode(Name)],"/")};
							_ ->
						{redirect_local, string:join(["","files",Dir, urlencode(Name), "preview"],"/")}
						end;
						{error, ErrorHTML} -> ErrorHTML
						end;
					Other ->
						template(io_lib:format("File upload failed? ~p", [Other]))
				end;
			Other -> Other
		end;
		["upload-image"] ->
%  			{ehtml, "Disaster, a total fucking disaster."};
		case yaws_multipart:read_multipart_form(A, []) of
			{done, Params} ->
				case dict:find("file", Params) of
					{ok, [{"filename", Fname}, {temp_file, Fpath} | _]} ->
						case handle_image_upload(utl(Fname), utl(Fpath)) of
							{ok, Dir, Name} ->
							case dict:find("token", Params) of
								{ok, TokenId} ->
									case get_token_by_id(TokenId) of
										{ok, Token} ->
											if Token#token.status == {nothing} ->
												TheToken = Token#token{status={ok, {"images", Dir, Name}}},
												tr(fun() -> mnesia:write(TheToken) end);
												true -> ok
											end;
										_ -> ok
									end;
								_ -> ok
							end,
							case A#arg.querydata of
								"simple" ->
							{redirect_local, string:join(["","images",Dir, urlencode(Name)],"/")};
								_ ->
							{redirect_local, string:join(["","images",Dir, urlencode(Name), "preview"],"/")}
							end;
							{error, ErrorHTML} -> ErrorHTML
						end;
					_Other ->
						template(io_lib:format("File upload failed? ~p", [Params]))
				end;
			Other -> Other
		end;
		["images", Name] -> out(A#arg{pathinfo=string:join(["images", ".", Name], "/")});
		["images", Dir, Name, "preview"] -> image_preview(Dir,Name, A#arg.headers#headers.host);
		["images", "thumbs", Dir, Name] -> 
			out(A#arg{pathinfo=string:join(["images", Dir, "thumb", Name], "/")});
		["images", Dir, "thumb", Name] ->
			case get_thumb("images", Dir, Name) of
				{ok, Mime} -> 
			[{header, {"X-Accel-Redirect", io_lib:format("/static/images/~ts/thumb/~ts", [ltb(Dir), ltb(Name)])}}, {header, {"Content-Type", Mime}}];
				{error, _Reason} -> {status, 404}
			end;
		["images", Dir, Name] ->
			case get_file("images", Dir, Name) of
				{ok, Mime, _} ->
			[{header, {"X-Accel-Redirect", io_lib:format("/static/images/~ts/~ts", [ltb(Dir), ltb(Name)])}},
			{header, {"Content-Type", Mime}}];
				{error, Reason} -> {status, 404}
			end;
		["files", Name] -> out(A#arg{pathinfo=string:join(["files", ".", Name], "/")});
		["files", Dir, Name, "preview"] -> file_preview(Dir, Name);
		["files", Dir, Name] ->
			case get_file("files", Dir, Name) of
				{ok, Mime, _} -> 
			[{header, {"X-Accel-Redirect", io_lib:format("/static/files/~ts/~ts", [ltb(Dir), ltb(Name)])}},
			{header, {"Content-Type", Mime}}];
				{error, _Reason} -> {status, 404}
			end;
		["texts", Name] -> {header, {"X-Accel-Redirect", io_lib:format("/static/texts/~ts", [ltb(Name)])}};
		["texts", Name, LanguagePristine] ->
			Language = capitalize(LanguagePristine),
			case lists:member(Language, get_paste_languages()) of
				false -> [{status, 404}, template(io_lib:format("No such language: ~ts!", [ltb(Language)]))];
				_ -> 
					Filename = filename:join([config(store_dir), "texts", Name]),
					case filelib:is_file(Filename) of
						true ->
						{ok, Text} = file:read_file(Filename),
						Highlighted = run_command(os:find_executable("Highlight"), ["-f", "-n", "-s", ltb(Language), ltb(Filename)]),
						[{header, {"Content-Type", "text/html"}}, template([
							Highlighted,
							{form, [{action, "/relight-text"}],[
								{input, [{name, "name"}, {type, "hidden"}, {value, Name}]},
								language_picker(Language),
								{input, [{type, "submit"}, {value, "Re-highlight"}]}
							]},
							{h2, [], "re-Dump"},
							text_upload_form(Text, Language)])
						];
						_ ->
							[{status, 404}]
					end
			end;
		["gentoken"] ->
			{H,M,S} = now(),
			random:seed(H,M,S),
			reap_tokens(),
			Token = genkey(83),
			tr(fun() -> mnesia:write(#token{id=Token, timestamp=get_unix_timestamp(), status={nothing}}) end),
			case lists:keyfind("format", 1, yaws_api:parse_query(A)) of
				{"format", "js"} -> {ehtml, io_lib:format("uploader.callback(\"~ts\");",[Token])};
				_ -> {ehtml, io_lib:format("~ts",[Token])}
			end;
		["gettoken", Token] ->
			case get_file_by_token_id(Token) of
				{ok, {Z,X,Y}} -> case lists:keyfind("format", 1, yaws_api:parse_query(A)) of
					{"format", "js"} -> case get_file(Z,X,Y) of
						{ok, _, _} ->
							{ehtml, "uploader.callback(\"" ++ Token ++ "\", " ++ json_callback_response({Z,X,Y}, A#arg.headers#headers.host) ++ ");"};
						_ -> {status, 404}
						end;
					_ -> {ehtml, string:join([Z,X,Y], "/")}
				end;
				_ -> {status, 404}
			end;
		["css.css"] -> {content, "text/css", css()};
		[] -> [{header, {"Content-Type", "text/html"}}, index()];
		_ -> {status, 404}
	end.

utl(Wtf) -> unicode:characters_to_list(list_to_binary(Wtf), utf8).
ltb(Wtf) -> binary_to_list(unicode:characters_to_binary(Wtf, unicode, utf8)).

urlencode(Wtf) -> http_uri:encode(ltb(Wtf)).
urldecode(Wtf) -> utl(http_uri:decode(Wtf)).

json_callback_response({Class, Dir, Name}, Host) ->
	Url = io_lib:format("http://~ts/~ts/~ts/~ts", [ltb(Host), ltb(Class), ltb(Dir), ltb(Name)]),
	Thumb = io_lib:format("http://~ts/~ts/~ts/thumb/~ts", [ltb(Host), ltb(Class), ltb(Dir), ltb(Name)]),
	Preview = io_lib:format("http://~ts/~ts/~ts/~ts/preview", [ltb(Host), ltb(Class), ltb(Dir), ltb(Name)]),
	Path = tuple_to_path({Class, Dir, Name}),
	{ok, FInfo} = file:read_file_info(Path),
	Size = FInfo#file_info.size,
	ThumbPath = filename:join([config(store_dir), Class, Dir, "thumb", Name]),
	
	BaseProps = [
		{url, Preview},
		{name, Name},
		{size, Size},
		{file, Url}
		],
	
	MoreProps = case Class of
		"images" ->
			Identify = run_command(os:find_executable("identify"), [ltb(Path)]),
			ImageProps = case re:run(Identify, "\\S+ \\S+ (\\d+)x(\\d+)", [{capture, all_but_first, list}]) of
				{match, [Width, Height]} -> [{pixelsize, [list_to_integer(Width), list_to_integer(Height)]}];
				_ -> []
			end,
			get_thumb(Class, Dir, Name),
			ThumbIdentify = run_command(os:find_executable("identify"), [ltb(ThumbPath)]),
			ThumbProps = case re:run(ThumbIdentify, "\\S+ \\S+ (\\d+)x(\\d+)", [{capture, all_but_first, list}]) of
				{match, [ThumbWidth, ThumbHeight]} -> [{thumbsize, [list_to_integer(ThumbWidth), list_to_integer(ThumbHeight)]}];
				_ -> []
			end,
			ImageProps ++ ThumbProps ++ [{thumb, Thumb}];
		_ -> []
	end,
	
	mochijson2:encode(BaseProps ++ MoreProps).

reap_tokens() ->
	TS = get_unix_timestamp() - 24 * 3600,
	tr(fun() -> 
		Victims = qlc:e(qlc:q([X || X <- mnesia:table(token), X#token.timestamp < TS, X#token.status == {nothing}])),
		lists:map(fun(A) -> mnesia:delete_object(A) end, Victims)
		end).

get_file_by_token_id(TokenId) ->
	case tr(fun() -> mnesia:read(token, TokenId) end) of
		{atomic, [Token]} -> case Token#token.status of
				{ok, Tuple} -> {ok, Tuple};
				_ -> {error, enoent}
			end;
		_ -> {error, enoent}
	end.

get_token_by_id(TokenId) -> 
	case tr(fun() -> mnesia:read(token, TokenId) end) of
		{atomic, [Token]} -> {ok, Token};
		_ -> {error, enoent}
	end.

random_one(List) ->
	lists:nth(random:uniform(length(List)), List).

genkey(Length) -> genkey("", Length).
genkey(Sofar, 0) -> Sofar;
genkey(Sofar, Length) ->
	UnVowels = "wrtpsdfghjklzxcvbnm",
	Vowels = "eyuioa",
	Char = case length(Sofar) rem 2 of
		0 -> random_one(Vowels);
		1 -> random_one(UnVowels)
	end,
	genkey([Char|Sofar], Length - 1).

gen_suitable_key(Length, Test) ->
	Key = genkey(Length),
	Result = Test(Key),
	if Result -> genkey(Length, Test);
		true -> Key
	end.

get_paste_languages() ->
	["Plaintext" | string:tokens(run_command(os:find_executable("Highlight"), ["-l"]), " ")].

capitalize([Fst | Rest]) -> [string:to_upper(Fst)] ++ string:to_lower(Rest).

handle_paste(Data, LanguagePristine) ->
	try
		Language = capitalize(LanguagePristine),
		case lists:member(Language, get_paste_languages()) of
			false ->
				throw({badlang, Language});
			_ -> ok
		end,
		{A,B,C} = now(),
		random:seed(A,B,C),
		FileName = gen_suitable_key(config(key_size), fun(K) -> filelib:is_file(filename:join([config(store_dir),"texts",K])) end),
		FullFileName = filename:join([config(store_dir),"texts",FileName]),
		filelib:ensure_dir(FullFileName),
		case Data of
			[{filename, _}, {temp_file, TempFileName}] ->
				file:rename(TempFileName, FullFileName);
			Content -> 
				file:write_file(FullFileName, Content)
		end,
		FileName
	of
		Filename -> {ok, Filename}
	catch
		throw: {badlang, Lang} ->
			{error, template(io_lib:format("Bad language name: ~ts", [Lang]))}
	end.

handle_upload(Fname, Fpath) ->
	try
%		soft_cleaner(),
		{A,B,C} = now(),
		random:seed(A,B,C),
		{ok, FInfo} = file:read_file_info(Fpath),
		Mfs = config(max_file_size),
		if	FInfo#file_info.size > Mfs ->
			throw({badsize, FInfo#file_info.size});
			true -> ok
		end,
		FnameCleaned = gsub(Fname, "(/|\s)+", "_"),
		{ok, F} = file:open("error.log", [append]),
			io:put_chars(F, io_lib:format("~p\n\n", [Fname])),
			file:close(F),

		DirName = gen_suitable_key(config(key_size), fun(K) -> filelib:is_file(filename:join([config(store_dir),"files",K, FnameCleaned])) end),
		DirPath = filename:join([config(store_dir),"files",DirName, FnameCleaned]),
		io:format("File: ~p ~ts~n", [DirPath, ltb(DirPath)]),
			filelib:ensure_dir(DirPath),
			file:copy(Fpath, DirPath),
			file:delete(Fpath),
			get_file("files", DirName, FnameCleaned),
			{ok, DirName, FnameCleaned}
	of
		Smth -> Smth
	catch
		throw: {badsize, Size} -> {error, template(io_lib:format("File is too big (~B)!", [Size]))}
	after
		file:delete(Fpath)
	end.

gsub(St, Pat, Rep) -> re:replace(St, Pat, Rep, [{return, list}, global, unicode]).

run_command_reader(Port, Timeout, Buffer) ->
	receive
		{Port, {data, Data}} ->
			run_command_reader(Port, Timeout, [Data | Buffer]);
		{Port, closed} -> Buffer;
		{'EXIT', Port, _Reason} -> Buffer;
		{Port, connected} -> Buffer;
		{Port, eof} -> Buffer
	after Timeout ->
		Buffer
	end.

run_command(Cmd, Args) ->
	Port = erlang:open_port({spawn_executable,Cmd}, [
		{args, Args},
		eof
	]),
	Results = lists:concat(lists:reverse(run_command_reader(Port, 20000, []))),
	try
		port_close(Port)
	of
		_ -> ok
	catch
		throw:{badarg, _} -> ok
	end,
	Results.

get_disk_space() -> list_to_integer(lists:nth(10, string:tokens(dumper:run_command(os:find_executable("df"), ["/"]), " "))).

soft_cleaner() ->
	Space = get_disk_space(),
	MinSpace = config(min_disk_space),
	if Space > MinSpace -> ok;
		true -> cleaner(config(max_disk_space))
	end.

handle_image_upload(Fname, Fpath) ->
	try
	%	soft_cleaner(),
		{A,B,C} = now(),
		random:seed(A,B,C),
		{ok, FInfo} = file:read_file_info(Fpath),
		Mis = config(max_image_size),
		if	FInfo#file_info.size > Mis ->
			throw({badsize, FInfo#file_info.size});
			true -> ok
		end,
		Type = lists:nth(2, string:tokens(run_command(os:find_executable("identify"),  [Fpath]), " ")),
		Member = lists:member(Type, ["JPEG", "PNG", "GIF"]),
		if Member -> ok;
			true -> throw(badimg)
		end,
		FnameCleaned = gsub(gsub(Fname, "(/|\s)+", "_"), "\.[^\.]+$", "." ++ string:to_lower(Type)),
		DirName = gen_suitable_key(config(key_size), fun(K) -> filelib:is_file(filename:join([config(store_dir),"images",K, FnameCleaned])) end),
		DirPath = filename:join([config(store_dir),"images",DirName, FnameCleaned]),
		filelib:ensure_dir(DirPath),
			file:copy(Fpath, DirPath),
			file:delete(Fpath),
			get_file("images", DirName, FnameCleaned),
			{ok, DirName, FnameCleaned}
	of
		Smth -> Smth
	catch
		error:_Other -> {error, template("Error: This is not an image!")};
		throw: {badsize, Size} -> template(io_lib:format("File is too big (~B)!", [Size]));
		throw:_Other -> {error, template("Error: This is not an image!")}
	after
		file:delete(Fpath)
	end.

template(Inside) -> {ehtml, {html, [], [{head, [], [
		{link, [
		{rel, "stylesheet"},
		{type, "text/css"},
		{href, "/files/hk-espresso.css"}
	]},
	{link, [
		{rel, "stylesheet"},
		{type, "text/css"},
		{href, "/css.css"}
	]},
	{link, [
		{rel, "icon"},
		{type, "image/png"},
		{href, "/files/ytoxuki/favicon.png"}
	]},
	{meta, [
		{'http-equiv', "Content-Type"},
		{content, "text/html; charset=utf-8"}
	]}
	]},
	{body, [], 
		[
			Inside,
			{hr},
			{p, [], [{a, [{href, "/"}], "dump.bitcheese.net"} ,io_lib:format(" - limits: file ~ts, image ~ts - dont hax kthx. ", [toMaxUnit(config(max_file_size)),toMaxUnit(config(max_image_size))]), {a, [{href, "http://bitcheese.net/wiki/dump"}], "Service description and donation information"}, " | ", {a, [{href, "http://bitcheese.net/wiki/dump/script"}], "Shell script"}, " | ", {a, [{href, "http://dump.bitcheese.net/files/abukaju/Bitcheese_Dump.apk"}], "Android app"}]},
			"Newsflash: ", newsflash()
		]
	}
	]}}.

newsflash() -> ["Use ", {a, [{href, "http://seedplz.bitcheese.net"}], "SeedPlz"}, " for distributing larger files"].

css() -> "body { 	background-color: #000000; 	color:white; } h1 { 	text-align: center; } .centered { 	text-align:center; } h2 { 	border-width:2px; 	border-color:#1C302C; /*	background-color: #3B2A5C; */} .text { 	border-width:2px; 	border-color:#1C302C; 	width:100%; } a:link { 	color:#00FF00; } a:visited { 	color:#00FF00; } a:hover { 	color:#55F9FF; } pre { white-space: pre-wrap }".

index() -> template([
		{h2, [], "Dump a picture"},
		{form, [
			{action, "/upload-image"},
			{method, "post"},
			{enctype, "multipart/form-data"} 		
		], [
			{input, [
				{type, "hidden"},
				{name, "MAX_FILE_SIZE"},
				{value, config(max_image_size)}
			]},
			{input, [
				{type, "file"},
				{name, "file"},
				{size, 50}
			]},
			{br},
			{input, [
				{value, "Dump!"},
				{type, "submit"}
			]}
		]},
		{h2, [], "Dump a file"},
		{form, [
			{action, "/upload-file"},
			{method, "post"},
			{enctype, "multipart/form-data"} 		
		], [
			{input, [
				{type, "hidden"},
				{name, "MAX_FILE_SIZE"},
				{value, config(max_file_size)}
			]},
			{input, [
				{type, "file"},
				{name, "file"},
				{size, 50}
			]},
			{br},
			{input, [
				{value, "Dump!"},
				{type, "submit"}
			]}
		]},
		{h2, [], "Dump a text"},
		text_upload_form()
	]).

text_upload_form() -> []. % text_upload_form("", none).

text_upload_form(Text, Language) ->
	{form, [
			{action, "/upload-text"},
			{method, "post"},
			{enctype, "multipart/form-data"} 		
		], [
			{textarea, [{cols, "80"}, {rows, "10"}, {name, "text"}], Text},
			{br},
			{input, [
				{value, "Dump!"},
				{type, "submit"}
			]},
			language_picker(Language)
		]}.

language_picker(Language) ->
	{select, [
		{name, "lang"}
		],
		lists:map(fun(L) -> case L of
			Language -> {option, [{selected, "1"}], L};
			_ -> {option, [], L}
			end
			end, get_paste_languages())
	}.

get_thumb(Class, Dir, Name) ->
	{ok, Mime, OrigFpath} = get_file(Class, Dir, Name), % TODO: handle return (don't do it)
	Fpath = filename:join([config(store_dir), Class, Dir, "thumb", Name]),
	case file:read_file_info(Fpath) of
		{ok, _} -> {ok, Mime};
		{error, enoent} -> % make a thumb, would ya
			filelib:ensure_dir(Fpath),
				run_command(os:find_executable("convert"),[
					"-size",
				lists:flatten(io_lib:format("~Bx~B", [config(thumb_width), config(thumb_height)])),
					"-resize",
					lists:flatten(io_lib:format("~Bx~B", [config(thumb_width), config(thumb_height)])),
					OrigFpath,
					"+profile",
					"\"*\"",
					Fpath]
				),
				{ok, Mime};
		{error, Reason} -> {error, Reason}
	end.

get_file_info(Class, Dir, Name) -> false.
% 	case tr(fun() -> mnesia:read({file, {Class, Dir, Name}}) end) of
% 		{atomic, [Info]} -> Info;
% 		{atomic, []} -> false;
% 		{error, enoent} -> false
% 	end.

get_file(Class, Dir, Name) ->
	Fname = filename:join([config(store_dir), Class, Dir, Name]),
	Present = case file:read_file_info(Fname) of
		{ok, _} -> true;
		{error, enoent} -> % file is absent, fetch it from S3?
			case tr(fun() -> mnesia:read({file, {Class, Dir, Name}}) end) of
				{atomic, [Info]} ->
					try
						filelib:ensure_dir(Fname),
						S3 = s3:new(#aws_credentials{ accessKeyId=config(s3_ak), secretAccessKey=config(s3_sak) }),
						S3:read_object(config(s3_bucket), string:join([Class,Dir,Name], "/"), [{save_response_to_file,Fname}]),
						S3:delete_object(config(s3_bucket), string:join([Class,Dir,Name], "/")),
						tr(fun() -> mnesia:write(Info#file{stored=true}) end),
						true
					of
						true -> true
					catch
						throw:{s3error,_,_} -> false
					end;
				_Other -> false
			end;
		{error, Reason} -> false
	end,
	if Present ->
		MimeType = case tr(fun() -> mnesia:read({file, {Class, Dir, Name}}) end) of
			{atomic, [File]} ->
				FileMod = File#file{accessed=get_unix_timestamp()},
				tr(fun() -> mnesia:write(FileMod) end),
				File#file.mime;
			_A ->
				{ok, FInfo} = file:read_file_info(Fname),
				Mime = get_mime_type(Fname),
				tr(fun() -> mnesia:write(#file{path={Class, Dir, Name}, mime=Mime, accessed=get_unix_timestamp(), size=FInfo#file_info.size, stored=true}) end),
				Mime
		end,
		{ok, MimeType, Fname};
	true -> {error, {enoent, {Class, Dir, Name}}}
	end.

file_preview(Dir, Name) ->
	FilePath = io_lib:format("/files/~ts/~ts", [ltb(Dir), ltb(Name)]),
	Content = case get_file_info("files", Dir, Name) of
		false -> "";
		Info -> io_lib:format(", ~ts, ~ts",[Info#file.mime, toMaxUnit(Info#file.size)])
	end,
	template(
	[{a, [{href, FilePath}], ltb(Name)}, Content]
	).

image_preview(Dir, Name, Host) ->
	ImgPath = io_lib:format("http://~ts/images/~ts/~ts", [ltb(Host), ltb(Dir), ltb(Name)]),
	ThumbPath = io_lib:format("http://~ts/images/~ts/thumb/~ts", [ltb(Host), ltb(Dir), ltb(Name)]),
	Content = case get_file_info("images", Dir, Name) of
		false -> "";
		Info -> io_lib:format("~ts, ~ts, ~ts",[ltb(Name), ltb(Info#file.mime), toMaxUnit(Info#file.size)])
	end,
	Input = fun(L, V) -> {tr, [], [
		{td, [], L},
		{td, [],
		{input, [
			{type, "text"},
			{size, "100"},
			{readonly, "readonly"},
			{value, V}
		]} }
		]}
	end,
	template(
		[Content, {br}, {a, [{href, ImgPath}],
		{img, 
			[{src, ThumbPath},
			{alt, "Your image"}
				]
			}},
			{table,
				[],
				[
					Input("Direct link: ", ImgPath), {br},
					Input("Thumbnail: ", ThumbPath), {br},
					Input("BBCODE: ", io_lib:format("[url=~ts][img]~ts[/img][/url]", [ImgPath, ThumbPath])), {br},
					Input("BBCODE (w/o preview): ", io_lib:format("[img]~ts[/img]", [ImgPath])), {br},
					Input("HTML: ", xmerl_lib:export_text(io_lib:format("<a href='~ts'><img src='~ts' alt='An Image' /></a>", [ImgPath, ThumbPath]))), {br},
					Input("HTML (w/o preview): ", xmerl_lib:export_text(io_lib:format("<img src='~ts' alt='An Image' />", [ImgPath]))), {br},
					Input("Textile: ",io_lib:format("!~ts!:~ts", [ThumbPath, ImgPath])), {br},
					Input("Textile (w/o preview): ",io_lib:format("!~ts!", [ImgPath]))
				]
			}
		]
	).

init([]) -> [];
init(L) -> lists:reverse(lists:nthtail(1,lists:reverse(L))).

make_tables() ->
	erlang:display(mnesia:create_schema([node()])),
	erlang:display(mnesia:start()),
	erlang:display(mnesia:create_table(file, [{attributes, record_info(fields, file)}, {disc_copies, [node()]}])),
	erlang:display(mnesia:create_table(token, [{attributes, record_info(fields, token)}, {disc_copies, [node()]}])),
	erlang:display(mnesia:stop()).
	
get_unix_timestamp() ->
	TS = now(),
	calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
		calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

% do(Q) ->
% 	F = fun() -> qlc:e(Q) end,
% 	{atomic, Val} = mnesia:transaction(F),
% 	Val.

tr(F) ->
	mnesia:transaction(F).

cleaner(WantedSpace) -> cleaner(WantedSpace, []).

cleaner(WantedSpace, SoFar) ->
	CurrentSpace = get_disk_space(),
	if
		CurrentSpace >= WantedSpace -> {ok, lists:reverse(SoFar)};
		true ->
			Query = qlc:q([ X || X <- mnesia:table(file), X#file.stored =:= true
				]),
			Sorted = qlc:sort(Query, [{order, (fun(X1, X2) ->
				if X1#file.accessed == X2#file.accessed ->
					X1#file.size > X2#file.size;
					true ->
					X1#file.accessed < X2#file.accessed
				end
				end)}]),
			case mnesia:transaction(fun() -> qlc:next_answers(qlc:cursor(Sorted), 1) end) of
				{atomic, [Victim]} ->
					{C, D, R} = Victim#file.path,
					PostMortem = reap(C, D, R),
					cleaner(WantedSpace, [PostMortem | SoFar]);
				_ -> {error, nomorefiles}
			end
	end.
	

reaper(Num) ->
% 	Date = get_unix_timestamp(),
	{atomic, Victims} = mnesia:transaction(fun() -> 
		Query = qlc:q([ X || X <- mnesia:table(file), X#file.stored =:= true
		]),
		Sorted = qlc:sort(Query, [{order, (fun(X1, X2) ->
			if X1#file.accessed == X2#file.accessed ->
				X1#file.size > X2#file.size;
				true ->
				X1#file.accessed < X2#file.accessed
			end
			end)}]),
		qlc:next_answers(qlc:cursor(Sorted), Num)
	end),
	lists:map(fun(V) -> 
		{C, D, R} = V#file.path,
		reap(C, D, R)
	end, Victims).
	

toucher(WildCard) ->
	Results = lists:map(fun(Path) ->
		case string:tokens(string:substr(Path,string:len(config(store_dir)) + 1), "/") of
			[Class, Dir | NameParts] -> 
				Name = string:join(NameParts, "/"),
				get_file(Class, Dir, Name);
			_ -> ok
		end
		end, filelib:wildcard(filename:join([config(store_dir), WildCard]))),
	{ok, length(lists:filter(fun(A) -> A /= ok end, Results))}.

gentle_toucher(WildCard) ->
	Results = lists:map(fun(Path) ->
		case string:tokens(string:substr(Path,string:len(config(store_dir)) + 1), "/") of
			[Class, Dir | NameParts] -> 
				Name = string:join(NameParts, "/"),
				case get_file_info(Class, Dir, Name) of
					false -> get_file(Class, Dir, Name);
					_ -> ok
				end;
			_ -> ok
		end
		end, filelib:wildcard(filename:join([config(store_dir), WildCard]))),
	{ok, length(lists:filter(fun(A) -> A /= ok end, Results))}.

reap(Class, Dir, Name) ->
	S3 = s3:new(#aws_credentials{ accessKeyId=config(s3_ak), secretAccessKey=config(s3_sak) }),
	Fname = filename:join([config(store_dir), Class, Dir, Name]),
	case file:open(Fname, [read, binary]) of
		{ok, Io} ->
			Info = get_file_info(Class, Dir, Name),
			Size = filelib:file_size(Fname),
			Reader = fun() -> file:read(Io, 1024 * 1024) end,
			try
			try
			S3:write_object(config(s3_bucket), string:join([Class, Dir, Name], "/"), {callback, Reader, Size}, Info#file.mime)
			of 
				A -> A
			catch
				throw:{s3error,"InvalidURI",_} -> ok % kill it with fire
			end,
			file:close(Io),
			file:delete(Fname),
			FileDir = filename:join([config(store_dir), Class, Dir]),
			case file:list_dir(FileDir) of
				{ok, Contents} ->
					case {Class, Contents} of
						{"images", ["thumb"]} ->
							rm_rf(FileDir);
						{"images", []} ->
							file:del_dir(FileDir);
						{_, []} -> 
							file:del_dir(FileDir);
						_ -> ok
					end;
				{error, Err} -> throw(Err)
			end,
			InfoMod = Info#file{stored=false},
			tr(fun() -> mnesia:write(InfoMod) end),
			{ok, InfoMod}
			of
				B -> B
			catch
				throw:Error -> {error, {s3_fucked_up, {Class, Dir, Name}, Error}};
				error:Error -> {error, {internal_s3_shit, {Class, Dir, Name}, Error}}
			end;
		{error, enoent} ->
			Info = get_file_info(Class, Dir, Name),
			InfoMod = Info#file{stored=false},
			tr(fun() -> mnesia:write(InfoMod) end),
			{ok, InfoMod};
		{error, eisdir} ->
			Info = get_file_info(Class, Dir, Name),
			tr(fun() -> mnesia:delete_object(Info) end),
			{ok, Info};
		{error, Reason} -> {error, Reason}
	end.

get_mime_type(Path) ->
	init(run_command(os:find_executable("file"), [
		"--mime-type",
		"-b",
		ltb(Path)
	])).

tuple_to_path({Class, Dir, Name}) ->
	filename:join([config(store_dir), Class, Dir, Name]).

rm_rf(FileName) ->
	Dir = filelib:is_dir(FileName),
	if	Dir ->
		{ok, Contents} = file:list_dir(FileName), 
		lists:map(fun(F) -> rm_rf(filename:join([FileName,F])) end, Contents),
		file:del_dir(FileName),
		ok;
	true -> 
		file:delete(FileName),
		ok
	end.

test() -> erlang:display(record_info(fields, token)).
