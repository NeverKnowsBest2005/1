import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FP32AdderTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "FP32Adder"

  /*
   * 将 Scala Float 的 IEEE-754 单精度编码
   * 转换成 32 位无符号整数，作为硬件输入。
   */
  def floatBits(x: Float): BigInt = {
    BigInt(
      java.lang.Float.floatToRawIntBits(x) & 0xffffffffL
    )
  }

  /*
   * 将 32 位 IEEE-754 编码转换为十六进制字符串，
   * 便于观察仿真结果。
   */
  def hex32(x: BigInt): String = {
    f"0x${x.longValue}%08X"
  }

  /*
   * 单组测试函数
   */
  def check(
      c: FP32Adder,
      a: Float,
      b: Float,
      expected: Float,
      name: String
  ): Unit = {

    val aBits        = floatBits(a)
    val bBits        = floatBits(b)
    val expectedBits = floatBits(expected)

    c.io.a.poke(aBits.U)
    c.io.b.poke(bBits.U)

    // 本实验模块为组合逻辑。
    // step 一拍主要用于使测试波形更加清晰。
    c.clock.step()

    val actualBits = c.io.y.peek().litValue

    println("----------------------------------------")
    println(s"Test : $name")
    println(s"A        = $a")
    println(s"B        = $b")
    println(s"Expected = $expected")
    println(s"A bits        = ${hex32(aBits)}")
    println(s"B bits        = ${hex32(bBits)}")
    println(s"Expected bits = ${hex32(expectedBits)}")
    println(s"Actual bits   = ${hex32(actualBits)}")

    // 自动判断结果
    c.io.y.expect(expectedBits.U)

    assert(expectedBits === actualBits)

    println("Result : PASS")
  }


  it should "pass all required FP32 addition tests" in {

    /*
     * 生成 VCD 波形。
     * 学生可以使用 GTKWave 观察内部数据变化。
     */
    test(new FP32Adder)
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>

        // =====================================================
        // 1. 基本正数加法
        // =====================================================

        check(
          c,
          1.0f,
          2.0f,
          3.0f,
          "Basic addition"
        )

        // =====================================================
        // 2. 同号、具有小数部分
        // =====================================================

        check(
          c,
          1.5f,
          2.25f,
          3.75f,
          "Fraction addition"
        )

        // =====================================================
        // 3. 异号加法
        // =====================================================

        check(
          c,
          5.5f,
          -2.25f,
          3.25f,
          "Different signs"
        )

        // =====================================================
        // 4. 完全抵消
        // =====================================================

        check(
          c,
          1.0f,
          -1.0f,
          0.0f,
          "Cancellation"
        )

        // =====================================================
        // 5. 两个负数
        // =====================================================

        check(
          c,
          -3.0f,
          -4.0f,
          -7.0f,
          "Negative addition"
        )

        // =====================================================
        // 6. 阶码不同：
        //    可以重点观察 expDiff 和 smallAligned
        // =====================================================

        check(
          c,
          100.0f,
          0.125f,
          100.125f,
          "Exponent alignment"
        )


        // =====================================================
        // 以下两组专门用于实验过程观察
        // =====================================================

        // -----------------------------------------------------
        // 7. 对阶观察
        //
        // 1.0   = 1.0 × 2^0
        // 0.125 = 1.0 × 2^-3
        //
        // 阶差应为 3
        // 小数尾数需要右移 3 位
        // -----------------------------------------------------

        check(
          c,
          1.0f,
          0.125f,
          1.125f,
          "Alignment observation"
        )

        // -----------------------------------------------------
        // 8. 左规格化观察
        //
        // 1.0 + (-0.875)
        // = 0.125
        //
        // 尾数相减以后会出现多个前导 0，
        // 需要左移并减小阶码。
        // -----------------------------------------------------

        check(
          c,
          1.0f,
          -0.875f,
          0.125f,
          "Left normalization observation"
        )

      }
  }
}
