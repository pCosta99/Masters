/*
 * This file was automatically generated by EvoSuite
 * Mon Jan 25 21:08:20 GMT 2021
 */

package MVC.Exceptions;

import org.junit.Test;
import static org.junit.Assert.*;
import MVC.Exceptions.NaoExisteException;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(mockJVMNonDeterminism = true, useVFS = true, useVNET = true, resetStaticState = true, separateClassLoader = true, useJEE = true) 
public class NaoExisteException_ESTest extends NaoExisteException_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      NaoExisteException naoExisteException0 = new NaoExisteException("", "");
  }

  @Test(timeout = 4000)
  public void test1()  throws Throwable  {
      NaoExisteException naoExisteException0 = new NaoExisteException();
  }

  @Test(timeout = 4000)
  public void test2()  throws Throwable  {
      NaoExisteException naoExisteException0 = new NaoExisteException("*U%t");
  }
}
