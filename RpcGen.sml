structure RpcGen =
struct
	open Proto;

	fun generateTypeName (TProtoMessage(MessageDef(name,_,_)), specialNames) =
		case List.find (fn x => x = name) specialNames of
			NONE => "protoMessage"
		|	SOME(n) = n
	  | generateTypeName _ = raise WrongFieldType

	fun generateRpcType (arg, ret, specialNames) =
		String.concat([generateTypeName(arg, specialNames),
					   " -> ",
					   generateTypeName(ret, specialNames)])

	fun generateRpcTypeSignature (RpcSignature(name, arg, ret), specialNames) =
		String.concat(["val ", StringManipulation.lowerFirst(name), " : ", generateRpcType(arg, ret, specialNames), "\n"])

	fun generateRpcCode (RpcSignature(name, _, _)) =
		String.concat(["fun ", StringManipulation.lowerFirst(name), "(msg) = doRPC \"", name, "\" msg\n"])

	fun generateStructureCodeFromRpcs(rpcs) =
		String.concat(map generateRpcCode rpcs)

	fun generateSignatureCodeFromRpcs(rpcs, specialNames) =
		String.concat(map (fn x => generateRpcTypeSignature(x, specialNames)) rpcs)

	fun packUpModule(moduleName,structureCode,signatureCode) =
		String.concat(["structure ", structName, " = \n",
			       "struct\n",
					structureCode,
			       "\nend : sig\n",
					signatureCode,
			       "\nend"])

	fun generateModule(name, rpcs, msgs) =
		let val specialNames = map messageDefName msgs
		    val code = generateStructureCodeFromRpcs(rpcs)
		    val spec = generateSignatureCodeFromRpcs(rpcs, specialNames)
		in
		    packUpModule(name,code,spec)
		end
end
