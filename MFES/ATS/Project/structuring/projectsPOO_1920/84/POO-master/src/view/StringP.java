package view;

public class StringP {
    private String str;

    public StringP() {
        this.str = "";
    }

    public StringP(String str) {
        this.str = str;
    }

    public String getStr() {
        return str;
    }

    private StringP setStr(String str) {
        this.str = str;
        return this;
    }

    public StringP repeat(int n){
        StringBuilder s = new StringBuilder();
        for(int i = 0; i < n; i++)
            s.append(this.str);
        return new StringP(s.toString());
    }

    public StringP append(String strA){
        this.str += strA;
        return this;
    }



    public StringP under(){
        return new StringP("\033[4m" + this.str).RESET();

    }

    public StringP blink(){
        return new StringP( "\033[5m" + this.str).RESET();
    }

    private StringP RESET(){
        return new StringP(this.str + "\033[0m");
    }

    public StringP hide_cursor(){
        return new StringP(this.str + "\033[?25l");
    }

    public StringP show_cursor(){
        return new StringP(this.str + "\033[?25h");
    }

    @Override
    public String toString() {
        return this.str;
    }
}