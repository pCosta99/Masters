package API;

import java.io.*;
import java.nio.ByteBuffer;

public class GenericMessage<T> implements Serializable {
    private String type;
    private T value;
    private Integer clock;

    public GenericMessage(String type, T value) {
        this.type = type;
        this.value = value;
    }

    public GenericMessage(String type, T value, Integer clock) {
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
            this.type = "ERRO";
            this.value = null;
        }
    }

    public T getValue(){
        return value;
    }

    public String getType() {
        return type;
    }

    public Integer getClock() {
        return this.clock;
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

    public boolean isPut(){
        return this.type.equals("put");
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Type: ").append(this.type).append("\n");
        //sb.append("Key: ").append(this.key).append("\n");
        //sb.append("Content: ").append(new String(this.value)).append("\n");
        return sb.toString();
    }
}
