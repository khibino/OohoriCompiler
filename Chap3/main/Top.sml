
structure Top =
struct

fun readAndPrintLoop inStream =
    let val () = ReadString.skipSpaces inStream
	val s  = ReadString.readString inStream
	val () = print (s ^ "\n")
    in readAndPrintLoop inStream end

fun topIn inStream =
    readAndPrintLoop inStream
    handle ReadString.EOF => ()

fun top file =
    let val inStream = TextIO.openIn file
    in readAndPrintLoop inStream;
       TextIO.closeIn inStream
    end
    handle ReadString.EOF => ()
end
