package protocol;

import com.google.protobuf.InvalidProtocolBufferException;

import java.io.IOException;
import java.io.InputStream;

import static protocol.Protocol.*;

public class MessageReader {
    public static LoginReply readLoginReply(InputStream in){
        LoginReply message = null;
        try {
            byte[] bytes = receive(in);
            message = LoginReply.parseFrom(bytes);
        } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
        }
        return message;
    }

    public static SignupReply readSignupReply(InputStream in){
        SignupReply message = null;
        try {
            byte[] bytes = receive(in);
            message = SignupReply.parseFrom(bytes);
        } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
        }
        return message;
    }

    public static OperationReply readOperationReply(InputStream in){
        OperationReply message = null;
        try {
            byte[] bytes = receive(in);
            message = OperationReply.parseFrom(bytes);
        } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
        }
        return message;
    }

    public static EntryMessage readEntryMessage(InputStream in){
        EntryMessage message = null;
        try {
            byte[] bytes = receive(in);
            message = EntryMessage.parseFrom(bytes);
        } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
        }
        return message;
    }

    private static byte[] receive(InputStream in){
        try {
            byte[] len = new byte[2048];
            int count;
            count = in.read(len);
            byte[] temp = new byte[count];
            if (count >= 0) System.arraycopy(len, 0, temp, 0, count);
            return temp;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
}
