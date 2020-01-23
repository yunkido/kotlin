// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: compiler/ir/serialization.common/src/KotlinIr.proto

package org.jetbrains.kotlin.backend.common.serialization.proto;

/**
 * Protobuf type {@code org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature}
 */
public final class IdSignature extends
    org.jetbrains.kotlin.protobuf.GeneratedMessageLite implements
    // @@protoc_insertion_point(message_implements:org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature)
    IdSignatureOrBuilder {
  // Use IdSignature.newBuilder() to construct.
  private IdSignature(org.jetbrains.kotlin.protobuf.GeneratedMessageLite.Builder builder) {
    super(builder);
    this.unknownFields = builder.getUnknownFields();
  }
  private IdSignature(boolean noInit) { this.unknownFields = org.jetbrains.kotlin.protobuf.ByteString.EMPTY;}

  private static final IdSignature defaultInstance;
  public static IdSignature getDefaultInstance() {
    return defaultInstance;
  }

  public IdSignature getDefaultInstanceForType() {
    return defaultInstance;
  }

  private final org.jetbrains.kotlin.protobuf.ByteString unknownFields;
  private IdSignature(
      org.jetbrains.kotlin.protobuf.CodedInputStream input,
      org.jetbrains.kotlin.protobuf.ExtensionRegistryLite extensionRegistry)
      throws org.jetbrains.kotlin.protobuf.InvalidProtocolBufferException {
    initFields();
    int mutable_bitField0_ = 0;
    org.jetbrains.kotlin.protobuf.ByteString.Output unknownFieldsOutput =
        org.jetbrains.kotlin.protobuf.ByteString.newOutput();
    org.jetbrains.kotlin.protobuf.CodedOutputStream unknownFieldsCodedOutput =
        org.jetbrains.kotlin.protobuf.CodedOutputStream.newInstance(
            unknownFieldsOutput, 1);
    try {
      boolean done = false;
      while (!done) {
        int tag = input.readTag();
        switch (tag) {
          case 0:
            done = true;
            break;
          default: {
            if (!parseUnknownField(input, unknownFieldsCodedOutput,
                                   extensionRegistry, tag)) {
              done = true;
            }
            break;
          }
          case 10: {
            org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature.Builder subBuilder = null;
            if (idsigCase_ == 1) {
              subBuilder = ((org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature) idsig_).toBuilder();
            }
            idsig_ = input.readMessage(org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature.PARSER, extensionRegistry);
            if (subBuilder != null) {
              subBuilder.mergeFrom((org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature) idsig_);
              idsig_ = subBuilder.buildPartial();
            }
            idsigCase_ = 1;
            break;
          }
          case 18: {
            org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature.Builder subBuilder = null;
            if (idsigCase_ == 2) {
              subBuilder = ((org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature) idsig_).toBuilder();
            }
            idsig_ = input.readMessage(org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature.PARSER, extensionRegistry);
            if (subBuilder != null) {
              subBuilder.mergeFrom((org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature) idsig_);
              idsig_ = subBuilder.buildPartial();
            }
            idsigCase_ = 2;
            break;
          }
          case 26: {
            org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature.Builder subBuilder = null;
            if (idsigCase_ == 3) {
              subBuilder = ((org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature) idsig_).toBuilder();
            }
            idsig_ = input.readMessage(org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature.PARSER, extensionRegistry);
            if (subBuilder != null) {
              subBuilder.mergeFrom((org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature) idsig_);
              idsig_ = subBuilder.buildPartial();
            }
            idsigCase_ = 3;
            break;
          }
          case 32: {
            idsigCase_ = 4;
            idsig_ = input.readInt64();
            break;
          }
          case 40: {
            idsigCase_ = 5;
            idsig_ = input.readInt32();
            break;
          }
        }
      }
    } catch (org.jetbrains.kotlin.protobuf.InvalidProtocolBufferException e) {
      throw e.setUnfinishedMessage(this);
    } catch (java.io.IOException e) {
      throw new org.jetbrains.kotlin.protobuf.InvalidProtocolBufferException(
          e.getMessage()).setUnfinishedMessage(this);
    } finally {
      try {
        unknownFieldsCodedOutput.flush();
      } catch (java.io.IOException e) {
      // Should not happen
      } finally {
        unknownFields = unknownFieldsOutput.toByteString();
      }
      makeExtensionsImmutable();
    }
  }
  public static org.jetbrains.kotlin.protobuf.Parser<IdSignature> PARSER =
      new org.jetbrains.kotlin.protobuf.AbstractParser<IdSignature>() {
    public IdSignature parsePartialFrom(
        org.jetbrains.kotlin.protobuf.CodedInputStream input,
        org.jetbrains.kotlin.protobuf.ExtensionRegistryLite extensionRegistry)
        throws org.jetbrains.kotlin.protobuf.InvalidProtocolBufferException {
      return new IdSignature(input, extensionRegistry);
    }
  };

  @java.lang.Override
  public org.jetbrains.kotlin.protobuf.Parser<IdSignature> getParserForType() {
    return PARSER;
  }

  private int bitField0_;
  private int idsigCase_ = 0;
  private java.lang.Object idsig_;
  public enum IdsigCase
      implements org.jetbrains.kotlin.protobuf.Internal.EnumLite {
    PUBLIC_SIG(1),
    PRIVATE_SIG(2),
    ACCESSOR_SIG(3),
    BUILTIN_SIG(4),
    SCOPED_LOCAL_SIG(5),
    IDSIG_NOT_SET(0);
    private int value = 0;
    private IdsigCase(int value) {
      this.value = value;
    }
    public static IdsigCase valueOf(int value) {
      switch (value) {
        case 1: return PUBLIC_SIG;
        case 2: return PRIVATE_SIG;
        case 3: return ACCESSOR_SIG;
        case 4: return BUILTIN_SIG;
        case 5: return SCOPED_LOCAL_SIG;
        case 0: return IDSIG_NOT_SET;
        default: throw new java.lang.IllegalArgumentException(
          "Value is undefined for this oneof enum.");
      }
    }
    public int getNumber() {
      return this.value;
    }
  };

  public IdsigCase
  getIdsigCase() {
    return IdsigCase.valueOf(
        idsigCase_);
  }

  public static final int PUBLIC_SIG_FIELD_NUMBER = 1;
  /**
   * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature public_sig = 1;</code>
   */
  public boolean hasPublicSig() {
    return idsigCase_ == 1;
  }
  /**
   * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature public_sig = 1;</code>
   */
  public org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature getPublicSig() {
    if (idsigCase_ == 1) {
       return (org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature) idsig_;
    }
    return org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature.getDefaultInstance();
  }

  public static final int PRIVATE_SIG_FIELD_NUMBER = 2;
  /**
   * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature private_sig = 2;</code>
   */
  public boolean hasPrivateSig() {
    return idsigCase_ == 2;
  }
  /**
   * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature private_sig = 2;</code>
   */
  public org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature getPrivateSig() {
    if (idsigCase_ == 2) {
       return (org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature) idsig_;
    }
    return org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature.getDefaultInstance();
  }

  public static final int ACCESSOR_SIG_FIELD_NUMBER = 3;
  /**
   * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature accessor_sig = 3;</code>
   */
  public boolean hasAccessorSig() {
    return idsigCase_ == 3;
  }
  /**
   * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature accessor_sig = 3;</code>
   */
  public org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature getAccessorSig() {
    if (idsigCase_ == 3) {
       return (org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature) idsig_;
    }
    return org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature.getDefaultInstance();
  }

  public static final int BUILTIN_SIG_FIELD_NUMBER = 4;
  /**
   * <code>optional int64 builtin_sig = 4;</code>
   */
  public boolean hasBuiltinSig() {
    return idsigCase_ == 4;
  }
  /**
   * <code>optional int64 builtin_sig = 4;</code>
   */
  public long getBuiltinSig() {
    if (idsigCase_ == 4) {
      return (java.lang.Long) idsig_;
    }
    return 0L;
  }

  public static final int SCOPED_LOCAL_SIG_FIELD_NUMBER = 5;
  /**
   * <code>optional int32 scoped_local_sig = 5;</code>
   */
  public boolean hasScopedLocalSig() {
    return idsigCase_ == 5;
  }
  /**
   * <code>optional int32 scoped_local_sig = 5;</code>
   */
  public int getScopedLocalSig() {
    if (idsigCase_ == 5) {
      return (java.lang.Integer) idsig_;
    }
    return 0;
  }

  private void initFields() {
  }
  private byte memoizedIsInitialized = -1;
  public final boolean isInitialized() {
    byte isInitialized = memoizedIsInitialized;
    if (isInitialized == 1) return true;
    if (isInitialized == 0) return false;

    if (hasPublicSig()) {
      if (!getPublicSig().isInitialized()) {
        memoizedIsInitialized = 0;
        return false;
      }
    }
    if (hasPrivateSig()) {
      if (!getPrivateSig().isInitialized()) {
        memoizedIsInitialized = 0;
        return false;
      }
    }
    if (hasAccessorSig()) {
      if (!getAccessorSig().isInitialized()) {
        memoizedIsInitialized = 0;
        return false;
      }
    }
    memoizedIsInitialized = 1;
    return true;
  }

  public void writeTo(org.jetbrains.kotlin.protobuf.CodedOutputStream output)
                      throws java.io.IOException {
    getSerializedSize();
    if (idsigCase_ == 1) {
      output.writeMessage(1, (org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature) idsig_);
    }
    if (idsigCase_ == 2) {
      output.writeMessage(2, (org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature) idsig_);
    }
    if (idsigCase_ == 3) {
      output.writeMessage(3, (org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature) idsig_);
    }
    if (idsigCase_ == 4) {
      output.writeInt64(
          4, (long)((java.lang.Long) idsig_));
    }
    if (idsigCase_ == 5) {
      output.writeInt32(
          5, (int)((java.lang.Integer) idsig_));
    }
    output.writeRawBytes(unknownFields);
  }

  private int memoizedSerializedSize = -1;
  public int getSerializedSize() {
    int size = memoizedSerializedSize;
    if (size != -1) return size;

    size = 0;
    if (idsigCase_ == 1) {
      size += org.jetbrains.kotlin.protobuf.CodedOutputStream
        .computeMessageSize(1, (org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature) idsig_);
    }
    if (idsigCase_ == 2) {
      size += org.jetbrains.kotlin.protobuf.CodedOutputStream
        .computeMessageSize(2, (org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature) idsig_);
    }
    if (idsigCase_ == 3) {
      size += org.jetbrains.kotlin.protobuf.CodedOutputStream
        .computeMessageSize(3, (org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature) idsig_);
    }
    if (idsigCase_ == 4) {
      size += org.jetbrains.kotlin.protobuf.CodedOutputStream
        .computeInt64Size(
            4, (long)((java.lang.Long) idsig_));
    }
    if (idsigCase_ == 5) {
      size += org.jetbrains.kotlin.protobuf.CodedOutputStream
        .computeInt32Size(
            5, (int)((java.lang.Integer) idsig_));
    }
    size += unknownFields.size();
    memoizedSerializedSize = size;
    return size;
  }

  private static final long serialVersionUID = 0L;
  @java.lang.Override
  protected java.lang.Object writeReplace()
      throws java.io.ObjectStreamException {
    return super.writeReplace();
  }

  public static org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature parseFrom(
      org.jetbrains.kotlin.protobuf.ByteString data)
      throws org.jetbrains.kotlin.protobuf.InvalidProtocolBufferException {
    return PARSER.parseFrom(data);
  }
  public static org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature parseFrom(
      org.jetbrains.kotlin.protobuf.ByteString data,
      org.jetbrains.kotlin.protobuf.ExtensionRegistryLite extensionRegistry)
      throws org.jetbrains.kotlin.protobuf.InvalidProtocolBufferException {
    return PARSER.parseFrom(data, extensionRegistry);
  }
  public static org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature parseFrom(byte[] data)
      throws org.jetbrains.kotlin.protobuf.InvalidProtocolBufferException {
    return PARSER.parseFrom(data);
  }
  public static org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature parseFrom(
      byte[] data,
      org.jetbrains.kotlin.protobuf.ExtensionRegistryLite extensionRegistry)
      throws org.jetbrains.kotlin.protobuf.InvalidProtocolBufferException {
    return PARSER.parseFrom(data, extensionRegistry);
  }
  public static org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature parseFrom(java.io.InputStream input)
      throws java.io.IOException {
    return PARSER.parseFrom(input);
  }
  public static org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature parseFrom(
      java.io.InputStream input,
      org.jetbrains.kotlin.protobuf.ExtensionRegistryLite extensionRegistry)
      throws java.io.IOException {
    return PARSER.parseFrom(input, extensionRegistry);
  }
  public static org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature parseDelimitedFrom(java.io.InputStream input)
      throws java.io.IOException {
    return PARSER.parseDelimitedFrom(input);
  }
  public static org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature parseDelimitedFrom(
      java.io.InputStream input,
      org.jetbrains.kotlin.protobuf.ExtensionRegistryLite extensionRegistry)
      throws java.io.IOException {
    return PARSER.parseDelimitedFrom(input, extensionRegistry);
  }
  public static org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature parseFrom(
      org.jetbrains.kotlin.protobuf.CodedInputStream input)
      throws java.io.IOException {
    return PARSER.parseFrom(input);
  }
  public static org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature parseFrom(
      org.jetbrains.kotlin.protobuf.CodedInputStream input,
      org.jetbrains.kotlin.protobuf.ExtensionRegistryLite extensionRegistry)
      throws java.io.IOException {
    return PARSER.parseFrom(input, extensionRegistry);
  }

  public static Builder newBuilder() { return Builder.create(); }
  public Builder newBuilderForType() { return newBuilder(); }
  public static Builder newBuilder(org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature prototype) {
    return newBuilder().mergeFrom(prototype);
  }
  public Builder toBuilder() { return newBuilder(this); }

  /**
   * Protobuf type {@code org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature}
   */
  public static final class Builder extends
      org.jetbrains.kotlin.protobuf.GeneratedMessageLite.Builder<
        org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature, Builder>
      implements
      // @@protoc_insertion_point(builder_implements:org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature)
      org.jetbrains.kotlin.backend.common.serialization.proto.IdSignatureOrBuilder {
    // Construct using org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature.newBuilder()
    private Builder() {
      maybeForceBuilderInitialization();
    }

    private void maybeForceBuilderInitialization() {
    }
    private static Builder create() {
      return new Builder();
    }

    public Builder clear() {
      super.clear();
      idsigCase_ = 0;
      idsig_ = null;
      return this;
    }

    public Builder clone() {
      return create().mergeFrom(buildPartial());
    }

    public org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature getDefaultInstanceForType() {
      return org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature.getDefaultInstance();
    }

    public org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature build() {
      org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature result = buildPartial();
      if (!result.isInitialized()) {
        throw newUninitializedMessageException(result);
      }
      return result;
    }

    public org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature buildPartial() {
      org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature result = new org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature(this);
      int from_bitField0_ = bitField0_;
      int to_bitField0_ = 0;
      if (idsigCase_ == 1) {
        result.idsig_ = idsig_;
      }
      if (idsigCase_ == 2) {
        result.idsig_ = idsig_;
      }
      if (idsigCase_ == 3) {
        result.idsig_ = idsig_;
      }
      if (idsigCase_ == 4) {
        result.idsig_ = idsig_;
      }
      if (idsigCase_ == 5) {
        result.idsig_ = idsig_;
      }
      result.bitField0_ = to_bitField0_;
      result.idsigCase_ = idsigCase_;
      return result;
    }

    public Builder mergeFrom(org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature other) {
      if (other == org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature.getDefaultInstance()) return this;
      switch (other.getIdsigCase()) {
        case PUBLIC_SIG: {
          mergePublicSig(other.getPublicSig());
          break;
        }
        case PRIVATE_SIG: {
          mergePrivateSig(other.getPrivateSig());
          break;
        }
        case ACCESSOR_SIG: {
          mergeAccessorSig(other.getAccessorSig());
          break;
        }
        case BUILTIN_SIG: {
          setBuiltinSig(other.getBuiltinSig());
          break;
        }
        case SCOPED_LOCAL_SIG: {
          setScopedLocalSig(other.getScopedLocalSig());
          break;
        }
        case IDSIG_NOT_SET: {
          break;
        }
      }
      setUnknownFields(
          getUnknownFields().concat(other.unknownFields));
      return this;
    }

    public final boolean isInitialized() {
      if (hasPublicSig()) {
        if (!getPublicSig().isInitialized()) {
          
          return false;
        }
      }
      if (hasPrivateSig()) {
        if (!getPrivateSig().isInitialized()) {
          
          return false;
        }
      }
      if (hasAccessorSig()) {
        if (!getAccessorSig().isInitialized()) {
          
          return false;
        }
      }
      return true;
    }

    public Builder mergeFrom(
        org.jetbrains.kotlin.protobuf.CodedInputStream input,
        org.jetbrains.kotlin.protobuf.ExtensionRegistryLite extensionRegistry)
        throws java.io.IOException {
      org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature parsedMessage = null;
      try {
        parsedMessage = PARSER.parsePartialFrom(input, extensionRegistry);
      } catch (org.jetbrains.kotlin.protobuf.InvalidProtocolBufferException e) {
        parsedMessage = (org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature) e.getUnfinishedMessage();
        throw e;
      } finally {
        if (parsedMessage != null) {
          mergeFrom(parsedMessage);
        }
      }
      return this;
    }
    private int idsigCase_ = 0;
    private java.lang.Object idsig_;
    public IdsigCase
        getIdsigCase() {
      return IdsigCase.valueOf(
          idsigCase_);
    }

    public Builder clearIdsig() {
      idsigCase_ = 0;
      idsig_ = null;
      return this;
    }

    private int bitField0_;

    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature public_sig = 1;</code>
     */
    public boolean hasPublicSig() {
      return idsigCase_ == 1;
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature public_sig = 1;</code>
     */
    public org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature getPublicSig() {
      if (idsigCase_ == 1) {
        return (org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature) idsig_;
      }
      return org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature.getDefaultInstance();
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature public_sig = 1;</code>
     */
    public Builder setPublicSig(org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature value) {
      if (value == null) {
        throw new NullPointerException();
      }
      idsig_ = value;

      idsigCase_ = 1;
      return this;
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature public_sig = 1;</code>
     */
    public Builder setPublicSig(
        org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature.Builder builderForValue) {
      idsig_ = builderForValue.build();

      idsigCase_ = 1;
      return this;
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature public_sig = 1;</code>
     */
    public Builder mergePublicSig(org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature value) {
      if (idsigCase_ == 1 &&
          idsig_ != org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature.getDefaultInstance()) {
        idsig_ = org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature.newBuilder((org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature) idsig_)
            .mergeFrom(value).buildPartial();
      } else {
        idsig_ = value;
      }

      idsigCase_ = 1;
      return this;
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.PublicIdSignature public_sig = 1;</code>
     */
    public Builder clearPublicSig() {
      if (idsigCase_ == 1) {
        idsigCase_ = 0;
        idsig_ = null;
        
      }
      return this;
    }

    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature private_sig = 2;</code>
     */
    public boolean hasPrivateSig() {
      return idsigCase_ == 2;
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature private_sig = 2;</code>
     */
    public org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature getPrivateSig() {
      if (idsigCase_ == 2) {
        return (org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature) idsig_;
      }
      return org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature.getDefaultInstance();
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature private_sig = 2;</code>
     */
    public Builder setPrivateSig(org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature value) {
      if (value == null) {
        throw new NullPointerException();
      }
      idsig_ = value;

      idsigCase_ = 2;
      return this;
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature private_sig = 2;</code>
     */
    public Builder setPrivateSig(
        org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature.Builder builderForValue) {
      idsig_ = builderForValue.build();

      idsigCase_ = 2;
      return this;
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature private_sig = 2;</code>
     */
    public Builder mergePrivateSig(org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature value) {
      if (idsigCase_ == 2 &&
          idsig_ != org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature.getDefaultInstance()) {
        idsig_ = org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature.newBuilder((org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature) idsig_)
            .mergeFrom(value).buildPartial();
      } else {
        idsig_ = value;
      }

      idsigCase_ = 2;
      return this;
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.FileLocalIdSignature private_sig = 2;</code>
     */
    public Builder clearPrivateSig() {
      if (idsigCase_ == 2) {
        idsigCase_ = 0;
        idsig_ = null;
        
      }
      return this;
    }

    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature accessor_sig = 3;</code>
     */
    public boolean hasAccessorSig() {
      return idsigCase_ == 3;
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature accessor_sig = 3;</code>
     */
    public org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature getAccessorSig() {
      if (idsigCase_ == 3) {
        return (org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature) idsig_;
      }
      return org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature.getDefaultInstance();
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature accessor_sig = 3;</code>
     */
    public Builder setAccessorSig(org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature value) {
      if (value == null) {
        throw new NullPointerException();
      }
      idsig_ = value;

      idsigCase_ = 3;
      return this;
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature accessor_sig = 3;</code>
     */
    public Builder setAccessorSig(
        org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature.Builder builderForValue) {
      idsig_ = builderForValue.build();

      idsigCase_ = 3;
      return this;
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature accessor_sig = 3;</code>
     */
    public Builder mergeAccessorSig(org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature value) {
      if (idsigCase_ == 3 &&
          idsig_ != org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature.getDefaultInstance()) {
        idsig_ = org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature.newBuilder((org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature) idsig_)
            .mergeFrom(value).buildPartial();
      } else {
        idsig_ = value;
      }

      idsigCase_ = 3;
      return this;
    }
    /**
     * <code>optional .org.jetbrains.kotlin.backend.common.serialization.proto.AccessorIdSignature accessor_sig = 3;</code>
     */
    public Builder clearAccessorSig() {
      if (idsigCase_ == 3) {
        idsigCase_ = 0;
        idsig_ = null;
        
      }
      return this;
    }

    /**
     * <code>optional int64 builtin_sig = 4;</code>
     */
    public boolean hasBuiltinSig() {
      return idsigCase_ == 4;
    }
    /**
     * <code>optional int64 builtin_sig = 4;</code>
     */
    public long getBuiltinSig() {
      if (idsigCase_ == 4) {
        return (java.lang.Long) idsig_;
      }
      return 0L;
    }
    /**
     * <code>optional int64 builtin_sig = 4;</code>
     */
    public Builder setBuiltinSig(long value) {
      idsigCase_ = 4;
      idsig_ = value;
      
      return this;
    }
    /**
     * <code>optional int64 builtin_sig = 4;</code>
     */
    public Builder clearBuiltinSig() {
      if (idsigCase_ == 4) {
        idsigCase_ = 0;
        idsig_ = null;
        
      }
      return this;
    }

    /**
     * <code>optional int32 scoped_local_sig = 5;</code>
     */
    public boolean hasScopedLocalSig() {
      return idsigCase_ == 5;
    }
    /**
     * <code>optional int32 scoped_local_sig = 5;</code>
     */
    public int getScopedLocalSig() {
      if (idsigCase_ == 5) {
        return (java.lang.Integer) idsig_;
      }
      return 0;
    }
    /**
     * <code>optional int32 scoped_local_sig = 5;</code>
     */
    public Builder setScopedLocalSig(int value) {
      idsigCase_ = 5;
      idsig_ = value;
      
      return this;
    }
    /**
     * <code>optional int32 scoped_local_sig = 5;</code>
     */
    public Builder clearScopedLocalSig() {
      if (idsigCase_ == 5) {
        idsigCase_ = 0;
        idsig_ = null;
        
      }
      return this;
    }

    // @@protoc_insertion_point(builder_scope:org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature)
  }

  static {
    defaultInstance = new IdSignature(true);
    defaultInstance.initFields();
  }

  // @@protoc_insertion_point(class_scope:org.jetbrains.kotlin.backend.common.serialization.proto.IdSignature)
}