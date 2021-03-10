/*
 * This file was automatically generated by EvoSuite
 * Mon Jan 25 21:18:34 GMT 2021
 */


import org.junit.Test;
import static org.junit.Assert.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(mockJVMNonDeterminism = true, useVFS = true, useVNET = true, resetStaticState = true, separateClassLoader = true, useJEE = true) 
public class DistanceCalculator_ESTest extends DistanceCalculator_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = DistanceCalculator.distance(1864.4428630075, 0.0, 0.0, 1864.4428630075);
      assertEquals(2636.720383134929, double0, 0.01);
  }

  @Test(timeout = 4000)
  public void test1()  throws Throwable  {
      double double0 = DistanceCalculator.distance(0.0, 0.0, 0.0, 1889.813);
      assertEquals(1889.813, double0, 0.01);
  }

  @Test(timeout = 4000)
  public void test2()  throws Throwable  {
      double double0 = DistanceCalculator.distance((-2007.4203062809), (-1.0), (-2007.4203062809), (-1.0));
      assertEquals(2837.506808963228, double0, 0.01);
  }

  @Test(timeout = 4000)
  public void test3()  throws Throwable  {
      double double0 = DistanceCalculator.distance(853.4943974778481, 853.4943974778481, 853.4943974778481, 853.4943974778481);
      assertEquals(0.0, double0, 0.01);
  }

  @Test(timeout = 4000)
  public void test4()  throws Throwable  {
      DistanceCalculator distanceCalculator0 = new DistanceCalculator();
  }
}