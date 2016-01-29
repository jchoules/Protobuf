structure ProtoParser =
struct
	open Parser
	open Proto
	exception UndefinedMessage of string
	exception PrimitiveArgOrReturn of string
	fun toType s =
		case s of
			"int32" => TInt32
		|	"int64" => TInt64
		|	"uint32" => TUInt32
		|	"uint64" => TUInt64
		|   "sint32" => TSInt32
		|	"sint64" => TSInt64
		|	"fixed32" => TFixed32
		|	"fixed64" => TFixed64
		|	"sfixed32" => TSFixed32
		|	"sfixed64" => TSFixed64
		|	"float" =>  TFloat
		|	"double" => TDouble
		| 	"string" => TString
		|	"bool"	=> TBool
		|	"bytes" => TBytes
		|	x => TProtoMessage (MessageDef(x,[],[]))
	val parseType = lift toType identifier
	val parseKey = wrap(
					ws(symbol #"="),
					ws(integer),
					ws(symbol #";"))
	val parseKeyVal = seq(ws(identifier),parseKey)
	val parseOptionType = seq(ws(word),ws(parseType))
	val parseField = lift (fn((opt,t),(name,key)) => FieldDef(Required,t,name,key))
						   (seq(parseOptionType,parseKeyVal))
	val parseFields = many(ws(parseField))
	val messageBody = wrap(symbol #"{",ws(parseFields),symbol #"}")
	val messageParser = precede(ws(keyword "message"),seq(ws(identifier),messageBody))
	val message = lift (fn (name,fields) => MessageDef(name,[],fields)) messageParser
	val messages = many(ws(message))
	(* TODO: This will hang if messages are (mutually) recursively defined. *)
	fun replaceWithProperMessage(message,messages) =
		case List.find (fn x => messageDefName(message) = messageDefName(x)) messages of
			SOME(x) => fixMessage messages x
		|	NONE => raise UndefinedMessage(messageDefName(message))
	and fixField messages field =
		let val FieldDef(opt,t,name,key) = field in
			case t of
				TProtoMessage msg => let val replacement = TProtoMessage(replaceWithProperMessage(msg,messages)) in
																FieldDef(opt,replacement,name,key)
															end
			|	_ => field
		end
	and fixMessage m(MessageDef(n,opt,fields)) = MessageDef(n,opt,map (fixField m) fields)
	fun fixMessages m ms = map (fixMessage m) ms
	fun parseMessages str = case parse(messages,str) of
								Failure x => Failure x
							|	Success(x) => Success(fixMessages x x)
									handle UndefinedMessage(m) => Failure(String.concat["Undefined message ",m])
	val messageParseTest = parseMessages("message Bla { required string a = 1;} message Test { required Bla t = 1;}")

	val rpcParser = precede(
						ws(keyword("rpc")),
					seq(
						ws(identifier),
					seq(
						wrap(symbol #"(",
							ws(parseType),
						symbol #")"),
					precede(
						ws(keyword("returns")),
					follow(
						wrap(symbol #"(",
							ws(parseType),
						symbol #")"),
						ws(symbol #";"))))))
	val rpc = lift (fn (name, (arg, returns)) => RpcSignature(name, arg, returns)) rpcParser
	val rpcs = many(ws(rpc))
	val serviceBody = wrap(symbol #"{",ws(rpcs),symbol #"}")
	val serviceParser = precede(
							ws(keyword("service")),
						seq(
							ws(identifier),
							serviceBody))
	val service = lift Service serviceParser
	val services = many(ws(service))
	fun fixRpc messages rpc =
		let val RpcSignature(name, arg, returns) = rpc in
			case (arg, returns) of
				(TProtoMessage argMsg, TProtoMessage retMsg) =>
					let
						val argReplacement = TProtoMessage(replaceWithProperMessage(argMsg,messages))
						val retReplacement = TProtoMessage(replaceWithProperMessage(retMsg,messages))
					in
						RpcSignature(name, argReplacement, retReplacement)
					end
			| _ => raise(PrimitiveArgOrReturn(name))
		end
	fun fixRpcs messages rpcs = map (fixRpc messages) rpcs
	fun fixService messages (Service(name, rpcs)) = Service(name, fixRpcs messages rpcs)
	fun fixServices messages services = map (fixService messages) services

	val messagesAndServices = manyAlt(ws(message), ws(service))
	fun parseMessagesAndServices str =
		case parse(messagesAndServices, str) of
				Failure(x) => Failure(x)
			|	Success(ms,ss) => let val fixedMs = fixMessages ms ms in
					Success(fixedMs, fixServices fixedMs ss)
						handle UndefinedMessage(m) => Failure(String.concat["Undefined message ",m])
							 | PrimitiveArgOrReturn(n) => Failure(String.concat["Argument and return types of ", n, " cannot be primitive"])
					 end

end :
sig
	val parseMessages : string -> (Proto.protoMessageDef list,string) Parser.result
end
