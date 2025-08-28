package tlc2.overrides;

import tlc2.value.impl.*;

import java.io.*;
import java.util.Base64;

public class Encoder {
    private static String encodeObjectToBase64(Object obj) throws IOException {
        ByteArrayOutputStream byteStream = new ByteArrayOutputStream();
        ObjectOutputStream objStream = new ObjectOutputStream(byteStream);
        objStream.writeObject(obj);
        objStream.close();
        return Base64.getEncoder().encodeToString(byteStream.toByteArray());
    }

    private static Object decodeObjectFromBase64(String base64) throws IOException, ClassNotFoundException {
        byte[] data = Base64.getDecoder().decode(base64);
        ObjectInputStream objStream = new ObjectInputStream(new ByteArrayInputStream(data));
        return objStream.readObject();
    }

	@TLAPlusOperator(identifier = "encode", module = "Encoder", warn = false)
    public static StringValue encode(Value object) throws IOException {
        return new StringValue(encodeObjectToBase64(object));
    }

    @TLAPlusOperator(identifier = "decode", module = "Encoder", warn = false)
    public static Value decode(StringValue base64) throws IOException, ClassNotFoundException {
        return (Value) decodeObjectFromBase64(base64.toUnquotedString());
    }

    @TLAPlusOperator(identifier = "hash", module = "Encoder", warn = false)
    public static IntValue hash(Value elem) {
        return IntValue.gen(elem.hashCode());
    }
}
