/*
 * This file was automatically generated by EvoSuite
 * Mon Jan 25 21:02:25 GMT 2021
 */

package MVC.Models.BaseModels;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import MVC.Models.BaseModels.GPS;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(mockJVMNonDeterminism = true, useVFS = true, useVNET = true, resetStaticState = true, separateClassLoader = true, useJEE = true) 
public class GPS_ESTest extends GPS_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test00()  throws Throwable  {
      GPS gPS0 = new GPS((-0.9656356558653689), (-0.9656356558653689));
      double double0 = gPS0.distancia(gPS0);
      assertEquals((-0.9656356558653689), gPS0.getX(), 0.01);
      assertEquals((-0.9656356558653689), gPS0.getY(), 0.01);
      assertEquals(0.0, double0, 0.01);
  }

  @Test(timeout = 4000)
  public void test01()  throws Throwable  {
      GPS gPS0 = new GPS();
      GPS gPS1 = new GPS(gPS0);
      gPS0.setY(1770.5855793);
      boolean boolean0 = gPS0.equals(gPS1);
      assertEquals(1770.5855793, gPS0.getY(), 0.01);
      assertFalse(boolean0);
  }

  @Test(timeout = 4000)
  public void test02()  throws Throwable  {
      GPS gPS0 = new GPS(79.245871302, 79.245871302);
      gPS0.setXY(317.057879, 79.245871302);
      assertEquals(317.057879, gPS0.getX(), 0.01);
  }

  @Test(timeout = 4000)
  public void test03()  throws Throwable  {
      GPS gPS0 = new GPS();
      double double0 = gPS0.getY();
      assertEquals(0.0, double0, 0.01);
      assertEquals(0.0, gPS0.getX(), 0.01);
  }

  @Test(timeout = 4000)
  public void test04()  throws Throwable  {
      GPS gPS0 = new GPS((-2174.5162020260395), (-2174.5162020260395));
      double double0 = gPS0.getY();
      assertEquals((-2174.5162020260395), gPS0.getX(), 0.01);
      assertEquals((-2174.5162020260395), double0, 0.01);
  }

  @Test(timeout = 4000)
  public void test05()  throws Throwable  {
      GPS gPS0 = new GPS(79.245871302, 79.245871302);
      double double0 = gPS0.getX();
      assertEquals(79.245871302, double0, 0.01);
      assertEquals(79.245871302, gPS0.getY(), 0.01);
  }

  @Test(timeout = 4000)
  public void test06()  throws Throwable  {
      GPS gPS0 = new GPS((-91.25088826), (-91.25088826));
      double double0 = gPS0.getX();
      assertEquals((-91.25088826), double0, 0.01);
      assertEquals((-91.25088826), gPS0.getY(), 0.01);
  }

  @Test(timeout = 4000)
  public void test07()  throws Throwable  {
      GPS gPS0 = new GPS();
      GPS gPS1 = new GPS(gPS0);
      gPS1.setY(291.4175716291);
      double double0 = gPS0.distancia(gPS1);
      assertEquals(291.4175716291, gPS1.getY(), 0.01);
      assertEquals(291.4175716291, double0, 0.01);
  }

  @Test(timeout = 4000)
  public void test08()  throws Throwable  {
      GPS gPS0 = new GPS(79.245871302, 79.245871302);
      GPS gPS1 = gPS0.clone();
      gPS0.setXY(317.057879, 79.245871302);
      boolean boolean0 = gPS1.equals(gPS0);
      assertEquals(317.057879, gPS0.getX(), 0.01);
      assertFalse(boolean0);
  }

  @Test(timeout = 4000)
  public void test09()  throws Throwable  {
      GPS gPS0 = new GPS((-2983.263088146759), (-2983.263088146759));
      // Undeclared exception!
      try { 
        gPS0.distancia((GPS) null);
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("MVC.Models.BaseModels.GPS", e);
      }
  }

  @Test(timeout = 4000)
  public void test10()  throws Throwable  {
      GPS gPS0 = null;
      try {
        gPS0 = new GPS((GPS) null);
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("MVC.Models.BaseModels.GPS", e);
      }
  }

  @Test(timeout = 4000)
  public void test11()  throws Throwable  {
      GPS gPS0 = new GPS();
      double double0 = gPS0.getX();
      assertEquals(0.0, double0, 0.01);
      assertEquals(0.0, gPS0.getY(), 0.01);
  }

  @Test(timeout = 4000)
  public void test12()  throws Throwable  {
      GPS gPS0 = new GPS(79.245871302, 79.245871302);
      double double0 = gPS0.getY();
      assertEquals(79.245871302, double0, 0.01);
      assertEquals(79.245871302, gPS0.getX(), 0.01);
  }

  @Test(timeout = 4000)
  public void test13()  throws Throwable  {
      GPS gPS0 = new GPS((-0.9656356558653689), (-0.9656356558653689));
      GPS gPS1 = gPS0.clone();
      assertEquals((-0.9656356558653689), gPS1.getY(), 0.01);
      
      gPS1.setXY(0.0, 0.0);
      boolean boolean0 = gPS1.equals(gPS0);
      assertFalse(boolean0);
  }

  @Test(timeout = 4000)
  public void test14()  throws Throwable  {
      GPS gPS0 = new GPS((-0.9656356558653689), (-0.9656356558653689));
      GPS gPS1 = gPS0.clone();
      boolean boolean0 = gPS1.equals(gPS0);
      assertTrue(boolean0);
      assertEquals((-0.9656356558653689), gPS1.getY(), 0.01);
      assertEquals((-0.9656356558653689), gPS1.getX(), 0.01);
  }

  @Test(timeout = 4000)
  public void test15()  throws Throwable  {
      GPS gPS0 = new GPS();
      boolean boolean0 = gPS0.equals("GPS: (-1047.80966,630.0419810941945)");
      assertEquals(0.0, gPS0.getX(), 0.01);
      assertEquals(0.0, gPS0.getY(), 0.01);
      assertFalse(boolean0);
  }

  @Test(timeout = 4000)
  public void test16()  throws Throwable  {
      GPS gPS0 = new GPS();
      boolean boolean0 = gPS0.equals(gPS0);
      assertEquals(0.0, gPS0.getY(), 0.01);
      assertEquals(0.0, gPS0.getX(), 0.01);
      assertTrue(boolean0);
  }

  @Test(timeout = 4000)
  public void test17()  throws Throwable  {
      GPS gPS0 = new GPS((-0.9656356558653689), (-0.9656356558653689));
      boolean boolean0 = gPS0.equals((Object) null);
      assertFalse(boolean0);
      assertEquals((-0.9656356558653689), gPS0.getX(), 0.01);
      assertEquals((-0.9656356558653689), gPS0.getY(), 0.01);
  }

  @Test(timeout = 4000)
  public void test18()  throws Throwable  {
      GPS gPS0 = new GPS((-0.9656356558653689), (-0.9656356558653689));
      gPS0.setX((-0.9656356558653689));
      assertEquals((-0.9656356558653689), gPS0.getX(), 0.01);
      assertEquals((-0.9656356558653689), gPS0.getY(), 0.01);
  }

  @Test(timeout = 4000)
  public void test19()  throws Throwable  {
      GPS gPS0 = new GPS();
      gPS0.clone();
      assertEquals(0.0, gPS0.getY(), 0.01);
      assertEquals(0.0, gPS0.getX(), 0.01);
  }

  @Test(timeout = 4000)
  public void test20()  throws Throwable  {
      GPS gPS0 = new GPS();
      String string0 = gPS0.toString();
      assertEquals("GPS: (0.0,0.0)", string0);
  }

  @Test(timeout = 4000)
  public void test21()  throws Throwable  {
      GPS gPS0 = new GPS();
      gPS0.setY((-538.8411181));
      GPS gPS1 = new GPS();
      boolean boolean0 = gPS0.equals(gPS1);
      assertEquals((-538.8411181), gPS0.getY(), 0.01);
      assertFalse(boolean0);
  }
}