// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: compiler/ir/serialization.common/src/KotlinIr.proto

package org.jetbrains.kotlin.backend.common.serialization.proto;

public interface IrWhenOrBuilder extends
    // @@protoc_insertion_point(interface_extends:org.jetbrains.kotlin.backend.common.serialization.proto.IrWhen)
    org.jetbrains.kotlin.protobuf.MessageLiteOrBuilder {

  /**
   * <code>repeated .org.jetbrains.kotlin.backend.common.serialization.proto.IrStatement branch = 1;</code>
   */
  java.util.List<org.jetbrains.kotlin.backend.common.serialization.proto.IrStatement> 
      getBranchList();
  /**
   * <code>repeated .org.jetbrains.kotlin.backend.common.serialization.proto.IrStatement branch = 1;</code>
   */
  org.jetbrains.kotlin.backend.common.serialization.proto.IrStatement getBranch(int index);
  /**
   * <code>repeated .org.jetbrains.kotlin.backend.common.serialization.proto.IrStatement branch = 1;</code>
   */
  int getBranchCount();

  /**
   * <code>optional int32 origin_name = 2;</code>
   */
  boolean hasOriginName();
  /**
   * <code>optional int32 origin_name = 2;</code>
   */
  int getOriginName();
}