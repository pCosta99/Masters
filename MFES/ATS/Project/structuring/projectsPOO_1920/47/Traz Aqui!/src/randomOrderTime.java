import java.util.Random;

public class randomOrderTime extends Random {
    private int queue; //<--- queue time; 0->NULL; 1->litle; 2-> +-; 3->big
    private int avgServiceTime;

    public randomOrderTime(int fila,int mediaTempoDeEspera){
        this.queue = fila; this.avgServiceTime = mediaTempoDeEspera;
    }

    public int getOrderProcessedTime(){
        int tempo = 0;
        Random moreORless = new Random();
        int moreLess = moreORless.nextInt(1);
        switch (queue){
            case 0: tempo = NullQueue(); break;
            case 1: tempo = litleQueue(); break;
            case 2: tempo = moreOrLessQueue(); break;
            case 3: tempo = muitaQueue(); moreLess=1 ;break;
        }
        int fin;
        if(moreLess==1) fin=avgServiceTime+tempo;
        else fin=avgServiceTime-tempo;
        return Math.abs(fin);
    }

    public int NullQueue(){
        Random moreORless = new Random();
        int tempo = 0;
        if(avgServiceTime>5) tempo = moreORless.nextInt(3);
        else tempo=avgServiceTime-1;
        return tempo;
    }
    public int litleQueue(){
        Random moreORless = new Random();
        int tempo = 0;
        if(avgServiceTime>0 && avgServiceTime<15) tempo = moreORless.nextInt(7);
        if(avgServiceTime>15 && avgServiceTime<30) tempo = moreORless.nextInt(12);
        else tempo=avgServiceTime-10;
        return tempo;
    }
    public int moreOrLessQueue(){
        Random moreORless = new Random();
        int tempo = 0;
        if(avgServiceTime>0 && avgServiceTime<15) tempo = moreORless.nextInt(10);
        if(avgServiceTime>15 && avgServiceTime<30) tempo = moreORless.nextInt(12);
        else tempo=avgServiceTime-10;
        return tempo;
    }
    public int muitaQueue(){
        Random moreORless = new Random();
        int tempo = 0;
        if(avgServiceTime>0 && avgServiceTime<15) tempo = moreORless.nextInt(12);
        if(avgServiceTime>15 && avgServiceTime<30) tempo = moreORless.nextInt(13);
        else tempo=avgServiceTime-20;
        return tempo;
    }


}
