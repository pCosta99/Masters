import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import hedgehog.control.Database;
import hedgehog.control.InterfaceController;
import hedgehog.control.QueryHandler;
import hedgehog.model.parser.Parser;
import hedgehog.view.GUI;

public class Main {
    public static void main(String[] args) throws IOException {
        Database db;

        try (
            final var db_in =
                new ObjectInputStream(new FileInputStream("db.ser"));
        ) {
            db = (Database) db_in.readObject();
        } catch (Exception e) {
            db = new Database();
            new Parser(db).parse("logs.txt").unwrap();
        }

        final var controller = new InterfaceController(db, new Parser(db), new GUI(new QueryHandler(db)));
        controller.startController();

        try (
            final var db_obj_out =
                new ObjectOutputStream(new FileOutputStream("db.ser"))
        ) {
            db_obj_out.writeObject(db);
        }
    }
}
