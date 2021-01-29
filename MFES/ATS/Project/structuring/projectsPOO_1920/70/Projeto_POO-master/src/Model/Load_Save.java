package Model;

import java.io.*;

/** Declaration of Class Load_Save which contains a File.
 *  This class saves the state of any given serializable object in to a given file.
 */

public class Load_Save {
    private File file;

    /* Class constructors*/
    public Load_Save(File file) {
        this.file = file;
    }
    public Load_Save() {
        this.file = new File("input_files/saveState.txt");
        try {
           file.createNewFile();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public Load_Save(String file) {
        this.file = new File(file);
    }

    /* Getters and Setters*/
    public File getFile() {
        return file;
    }
    public void setFile(File file) {
        this.file = file;
    }

    /* Saves an Object into a file*/
    public void save(Object s) throws IOException {
        ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(file));
        o.writeObject(s);
        o.flush();
        o.close();
    }

    /* Loads the information of the file into an object*/
    public Object load() throws IOException, ClassNotFoundException {
        FileInputStream in = new FileInputStream(file);
        Object s;
        if(in.available() != 0) {
            ObjectInputStream o = new ObjectInputStream(in);
            s = o.readObject();
            o.close();
        }else {
            s = null;
        }
        in.close();
        return s;
    }

    /* Deletes all the information in the file*/
    public void resetFileInfo(){
        this.file.delete();
        try {
            this.file.createNewFile();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
