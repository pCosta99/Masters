package Parser;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import MVC.Model.Model;

import java.io.FileInputStream;

public class GestVendas {

    public static boolean save(String file, Model model) {
        
        File fos = new File(file);
        try {
            if (fos.createNewFile() == true) {
                FileOutputStream fas = new FileOutputStream(fos);
                ObjectOutputStream oos = new ObjectOutputStream(fas);

                oos.writeObject(model);

                oos.close();
            }
        } catch (FileNotFoundException e) {
            return false;
        } catch (IOException e) {
            return false;
        }
        return true;
    }

    public static Model read(String file) {
        Model a = null;
        try {
            FileInputStream fas = new FileInputStream(file);
            ObjectInputStream ois = new ObjectInputStream(fas);

            a = new Model((Model) ois.readObject());

            ois.close();
        } catch (FileNotFoundException e) {
            return null;
        } catch (IOException e) {
            return null;
        } catch (ClassNotFoundException e) {
            return null;
        }

        return a;
    }
}