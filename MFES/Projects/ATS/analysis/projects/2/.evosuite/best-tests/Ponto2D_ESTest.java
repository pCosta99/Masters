/*
 * This file was automatically generated by EvoSuite
 * Mon Jan 25 20:51:37 GMT 2021
 */


import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.List;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(mockJVMNonDeterminism = true, useVFS = true, useVNET = true, resetStaticState = true, separateClassLoader = true, useJEE = true) 
public class Ponto2D_ESTest extends Ponto2D_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test00()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D(1.0, 1.0);
      ponto2D0.setX((-154.35));
      Ponto2D ponto2D1 = new Ponto2D();
      boolean boolean0 = ponto2D0.equals(ponto2D1);
      assertEquals((-154.35), ponto2D0.getX(), 0.01);
      assertFalse(boolean0);
  }

  @Test(timeout = 4000)
  public void test01()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D((-1197.370895301458), (-1197.370895301458));
      Ponto2D ponto2D1 = new Ponto2D(0.0, 0.0);
      ponto2D0.move(ponto2D1);
      assertEquals(0.0, ponto2D0.getY(), 0.01);
  }

  @Test(timeout = 4000)
  public void test02()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D();
      double double0 = ponto2D0.getY();
      assertEquals(0.0, double0, 0.01);
      assertEquals(0.0, ponto2D0.getX(), 0.01);
  }

  @Test(timeout = 4000)
  public void test03()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D(0.0, 1.0);
      double double0 = ponto2D0.getY();
      assertEquals(1.0, double0, 0.01);
      assertEquals(0.0, ponto2D0.getX(), 0.01);
  }

  @Test(timeout = 4000)
  public void test04()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D(1.0, 1.0);
      double double0 = ponto2D0.getX();
      assertEquals(1.0, double0, 0.01);
      assertEquals(1.0, ponto2D0.getY(), 0.01);
  }

  @Test(timeout = 4000)
  public void test05()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D();
      ponto2D0.move((-1.0), 0.0);
      double double0 = ponto2D0.getX();
      assertEquals((-1.0), double0, 0.01);
  }

  @Test(timeout = 4000)
  public void test06()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D();
      ponto2D0.move((-1277.228100483), (-1277.228100483));
      Ponto2D ponto2D1 = new Ponto2D();
      double double0 = ponto2D1.distance(ponto2D0);
      assertEquals((-1277.228100483), ponto2D0.getX(), 0.01);
      assertEquals(1806.2733019470847, double0, 0.01);
  }

  @Test(timeout = 4000)
  public void test07()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D((-1.0), 1439.47150818355);
      double double0 = ponto2D0.distance((-1.0), 1439.47150818355);
      assertEquals(0.0, double0, 0.01);
  }

  @Test(timeout = 4000)
  public void test08()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D(1.0, 1.0);
      Ponto2D ponto2D1 = ponto2D0.clone();
      ponto2D0.move((-1881.3169502), 1.0);
      ponto2D1.move((-1881.3169502), 0.0);
      boolean boolean0 = ponto2D0.equals(ponto2D1);
      assertEquals((-1881.3169502), ponto2D1.getX(), 0.01);
      assertFalse(boolean0);
  }

  @Test(timeout = 4000)
  public void test09()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D();
      ponto2D0.move((-1277.228100483), (-1277.228100483));
      ponto2D0.clone();
      assertEquals((-1277.228100483), ponto2D0.getY(), 0.01);
  }

  @Test(timeout = 4000)
  public void test10()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D();
      // Undeclared exception!
      try { 
        ponto2D0.move((Ponto2D) null);
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("Ponto2D", e);
      }
  }

  @Test(timeout = 4000)
  public void test11()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D(0.0, 1.0);
      // Undeclared exception!
      try { 
        ponto2D0.distance((Ponto2D) null);
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("Ponto2D", e);
      }
  }

  @Test(timeout = 4000)
  public void test12()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D();
      // Undeclared exception!
      try { 
        ponto2D0.closest((List<Ponto2D>) null);
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("Ponto2D", e);
      }
  }

  @Test(timeout = 4000)
  public void test13()  throws Throwable  {
      Ponto2D ponto2D0 = null;
      try {
        ponto2D0 = new Ponto2D((Ponto2D) null);
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("Ponto2D", e);
      }
  }

  @Test(timeout = 4000)
  public void test14()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D(0.0, (-1700.0273));
      double double0 = ponto2D0.getX();
      assertEquals(0.0, double0, 0.01);
      assertEquals((-1700.0273), ponto2D0.getY(), 0.01);
  }

  @Test(timeout = 4000)
  public void test15()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D(0.0, (-1700.0273));
      double double0 = ponto2D0.getY();
      assertEquals(0.0, ponto2D0.getX(), 0.01);
      assertEquals((-1700.0273), double0, 0.01);
  }

  @Test(timeout = 4000)
  public void test16()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D(0.0, (-1700.0273));
      assertEquals(0.0, ponto2D0.getX(), 0.01);
      
      ponto2D0.setX((-1700.0273));
      Ponto2D ponto2D1 = new Ponto2D(ponto2D0);
      ponto2D1.move((-1700.0273), 0.0);
      boolean boolean0 = ponto2D0.equals(ponto2D1);
      assertFalse(boolean0);
  }

  @Test(timeout = 4000)
  public void test17()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D(0.0, (-1700.0273));
      Ponto2D ponto2D1 = new Ponto2D(ponto2D0);
      boolean boolean0 = ponto2D1.equals(ponto2D0);
      assertEquals((-1700.0273), ponto2D0.getY(), 0.01);
      assertEquals(0.0, ponto2D0.getX(), 0.01);
      assertTrue(boolean0);
  }

  @Test(timeout = 4000)
  public void test18()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D(0.0, (-1700.0273));
      boolean boolean0 = ponto2D0.equals("(-1700.0273,-1700.0273)");
      assertFalse(boolean0);
      assertEquals((-1700.0273), ponto2D0.getY(), 0.01);
      assertEquals(0.0, ponto2D0.getX(), 0.01);
  }

  @Test(timeout = 4000)
  public void test19()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D();
      boolean boolean0 = ponto2D0.equals((Object) null);
      assertFalse(boolean0);
      assertEquals(0.0, ponto2D0.getX(), 0.01);
      assertEquals(0.0, ponto2D0.getY(), 0.01);
  }

  @Test(timeout = 4000)
  public void test20()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D(0.0, (-1700.0273));
      boolean boolean0 = ponto2D0.equals(ponto2D0);
      assertEquals(0.0, ponto2D0.getX(), 0.01);
      assertEquals((-1700.0273), ponto2D0.getY(), 0.01);
      assertTrue(boolean0);
  }

  @Test(timeout = 4000)
  public void test21()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D();
      double double0 = ponto2D0.distance(ponto2D0);
      assertEquals(0.0, ponto2D0.getY(), 0.01);
      assertEquals(0.0, ponto2D0.getX(), 0.01);
      assertEquals(0.0, double0, 0.01);
  }

  @Test(timeout = 4000)
  public void test22()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D();
      ponto2D0.setY((-1277.228100483));
      assertEquals((-1277.228100483), ponto2D0.getY(), 0.01);
  }

  @Test(timeout = 4000)
  public void test23()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D(0.0, (-1700.0273));
      double double0 = ponto2D0.distance((-1700.0273), (-1700.0273));
      assertEquals(1700.0273, double0, 0.01);
      assertEquals(0.0, ponto2D0.getX(), 0.01);
  }

  @Test(timeout = 4000)
  public void test24()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D();
      Ponto2D ponto2D1 = ponto2D0.clone();
      ponto2D1.setX((-1978.7));
      boolean boolean0 = ponto2D0.equals(ponto2D1);
      assertEquals((-1978.7), ponto2D1.getX(), 0.01);
      assertFalse(boolean0);
  }

  @Test(timeout = 4000)
  public void test25()  throws Throwable  {
      Ponto2D ponto2D0 = new Ponto2D();
      String string0 = ponto2D0.toString();
      assertEquals("(0.0,0.0)", string0);
  }
}
