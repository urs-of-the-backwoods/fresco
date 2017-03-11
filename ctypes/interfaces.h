// all the interface used in the fresco components

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

//
// Fresco Abstractions and Types
//

// a CBor Message, bytes and length

typedef uint8_t* FrMsg;       
typedef uint32_t FrMsgLength;

/*
    Components will be used later and they can be either an Item
    or an Item Property. They have a type, which is denoted by
    an uint64_t.
*/

typedef uint64_t FrComponentType;   
typedef FrComponentType FrItemType;
typedef FrComponentType FrPropertyType;
typedef FrComponentType FrEventType;

/*
    to get something, we can send a message to, we need a receiving item
    and a function, which takes this item and a message. This will be 
    the base abstraction of Fresco.

    The items will have a type, called component type.

    The combination of typed item with a message function can be seen as the
    most simple receiver.

*/

typedef void* FrItem;
typedef void (*FrMessageFn) (FrItem it, FrMsg m, FrMsgLength l);

typedef struct {
    FrItem item;
    FrMessageFn sendFn;
} FrReceiver;

// callback with entity pointers
// pub type FrMessageFn2 = extern "C" fn (item: EntityPointer, ct: FrPropertyType, msg: FrMsg, ml: FrMsgLength); 
typedef void* FrEntity;
typedef void (FrMessageFn2) (FrEntity e, FrEventType pr, FrMsg m, FrMsgLength l);

/*
    giornata (object lib libraries) interface

    A C++ object library exhibits this interface, to enable working 
    with the exposed items.

    There are some simple functions to create and destroy items. There
    is a function to get the function for setting properties of an item.
    And there is a function to set a property change callback. 
*/

typedef struct {

    FrItem (*gioCreateItem) (FrItemType ct, FrMsg m, FrMsgLength l);
    void (*gioDestroyItem) (FrItemType ct, FrItem it);
    FrMessageFn (*gioGetMsgSender) (FrItemType ob, FrPropertyType pr); 
    void (*gioRegisterMsgReceiver) (FrItemType ob, FrEventType ev, FrItem it, FrEntity rcv, FrMessageFn2);

} FrGiornateInterface;






//
// generell Fresco types
//

typedef int32_t FrError;
typedef void* FrItem;               // Object Library Item, created in C++ domain specific layer
typedef void* FrReceiver;           // Msg Receiver 
typedef void* FrEntity;             // Entity, created in Intoncao, Entity runtime, contains multiple components
typedef void* FrSystem;             // A specific domain system, handling entites within this domain

typedef char* FrGiornataEnv;        // name of Giornata library environment variable

typedef FrError (*FrMsgFn) (FrMsg m, FrMsgLength l);
typedef FrError (*FrMsgEntityFn) (FrEntity e, FrComponentType ct, FrMsg m, FrMsgLength l);

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


#ifdef __cplusplus
}
#endif
