package Protocol;

import java.io.*;
import java.nio.ByteBuffer;

public class GenericMessage<T> implements Serializable {
    public enum TYPE {
        PUT,
        GET,
        GET_SS,
        GET_CLOCK,
        PUT_DONE,
        REPLY,
        ERROR
    }
    private TYPE type;
    private T value;
    private Integer clock = 0;

    public GenericMessage(TYPE type, T value) {
        this.type = type;
        this.value = value;
    }

    public GenericMessage(TYPE type, T value, Integer clock) {
        this.type = type;
        this.value = value;
        this.clock = clock;
    }

    public GenericMessage(ByteBuffer buff){
        try {
            ByteArrayInputStream in = new ByteArrayInputStream(buff.array());
            ObjectInputStream is = new ObjectInputStream(in);
            GenericMessage<T> gm = (GenericMessage) is.readObject();
            this.type = gm.getType();
            this.value = gm.getValue();
            this.clock = gm.getClock();
        }
        catch (Exception e) {
            e.printStackTrace();
            this.type = TYPE.ERROR;
            this.value = null;
        }
    }

    public T getValue() {
        return value;
    }

    public Integer getClock() {
        return this.clock;
    }

    public TYPE getType() {
        return type;
    }

    public ByteBuffer serialize(){
        try {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            ObjectOutputStream os = new ObjectOutputStream(out);
            os.writeObject(this);

            ByteBuffer buff = ByteBuffer.allocate(out.size()+4);
            buff.putInt(out.size());
            buff.put(out.toByteArray());

            buff.flip();
            return buff;
        } catch (IOException e) {
            e.printStackTrace();
            return ByteBuffer.wrap("ERROR".getBytes());
        }
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Type: ").append(this.type).append("\n");
        return sb.toString();
    }
}
