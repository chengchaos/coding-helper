package vip.chengchao.helper.scala

import java.util.Objects

/**
  * <p>
  * <strong>
  * 用一句话描述功能
  * </strong><br /><br />
  * 如题。
  * </p>
  *
  * @author chengchaos[as]Administrator - 2019/6/12 11:56 <br />
  * @see 【相关类方法】
  * @since 1.1.0
  */
object CodingHelper {
  /**
    * create set and return
    *
    * @param whatever   创建の新对象
    * @param setOperate 设置
    * @tparam A 类型哦
    * @return A
    */
  def csar[A](whatever: A)(setOperate: A => Any): A = {
    setOperate(whatever)
    whatever
  }

  /**
    * Non Null Or Else
    *
    * @param whatever 一个对象
    * @param other    如果为空则返回另外的
    * @tparam A 类型
    * @return
    */
  def nneo[A](whatever: A)(other: => A): A = {
    if (Objects nonNull whatever) whatever
    else other
  }

  /**
    *
    * https://blog.csdn.net/weixin_33815613/article/details/91156018
    *
    * @param closeable 带有 close 方法的东东
    * @param callback  回调
    * @tparam A 输入类型
    * @tparam B 输出类型
    * @return
    */
  def withClose[A <: {def close() : Unit}, B](closeable: A)(callback: A => B): B = {
    try {
      callback(closeable)
    }
    finally {
      closeable.close()
    }
  }

}
