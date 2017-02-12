// all the interface used in the fresco components

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

//
// generell Fresco types
//

typedef int32_t FrError;
typedef unsigned char* FrMsg;       // A CBOR Message
typedef uint32_t FrMsgLength;
typedef uint64_t FrComponentType;   // A component can be an Object or a Property
typedef void* FrItem;               // Object Library Item, created in C++ domain specific layer
typedef void* FrReceiver;           // Msg Receiver 
typedef void* FrEntity;             // Entity, created in Intoncao, Entity runtime, contains multiple components
typedef void* FrSystem;             // A specific domain system, handling entites within this domain

typedef char* FrGiornataEnv;        // name of Giornata library environment variable

typedef FrError (*FrMsgFn) (FrMsg m, FrMsgLength l);
typedef FrError (*FrMsgEntityFn) (FrEntity e, FrComponentType ct, FrMsg m, FrMsgLength l);

typedef FrError (*FrMsgItemFn) (FrItem it, FrMsg m, FrMsgLength l);
typedef FrError (*FrMsgItemFn2) (FrItem it, FrComponentType ct, FrMsg m, FrMsgLength l);


//
// intonaco (entity  component system runtime) interface
//

typedef struct {

    FrError (*inEntityCreate) (FrMsg m, FrMsgLength l, FrEntity *e);                // Msg contains a CBOR array from arrays [u64, bs] 
    FrError (*inEntityReadComponent) (FrEntity e, FrComponentType ct, FrMsgFn f);               // also names are "Entity", read write works per component
    FrError (*inEntityWriteComponent) (FrEntity e, FrComponentType ct, FrMsg m, FrMsgLength l);
    FrError (*inEntityReadId) (FrEntity e, FrMsgFn f);  
    FrError (*inEntityDestroy) (FrEntity e);

    FrError (*inObjectLibSystemInit) (FrGiornataEnv g, msgPointer m, msgLength l, FrSystem *ps);       // Msg contains specific system creation parameters
    FrError (*inObjectLibSystemAddEntity) (FrSystem s, FrEntity e);
    FrError (*inObjectLibSystemRemoveEntity) (FrSystem s, FrEntity e);
    FrError (*inObjectLibSystemShutdown) (FrSystem s);
    FrError (*inObjectLibSystemStep) (FrSystem s);                                    // runs one cycle of system (control over Thread needed)

    FrError (*inCallbackSystemInit) (FrSystem *ps);
    FrError (*inCallbackSystemRegisterReceiver) (FrSystem s, FrEntity e, FrComponentType ct, FrMsgEntityFn f);  
    FrError (*inCallbackSystemShutdown) (FrSystem s);
    FrError (*inCallbackSystemStep) (FrSystem s);

    char* inErrorMessage(FrError);

} FrIntonacoInterface;

//
// giornata (object lib libraries) interface
//

typedef struct {

    FrError (*gioCreateItem) (FrComponentType ct, FrMsg m, FrMsgLength l, FrItem *fri);
    FrError (*gioDestroyItem) (FrComponentType ct, FrItem it);
    FrError (*gioGetMsgSender) (FrComponentType ob, FrComponentType pr, FrMsgItemFn *f);                            // ct's: first is object, snd property'
    FrError (*gioRegisterMsgReceiver) (FrComponentType ct, FrComponentType ev, FrItem it, FrReceiver rcv, FrMsgItemFn2);
    char * (*gioErrorMessage) (FrError);

} FrGiornateInterface;

#ifdef __cplusplus
}
#endif
