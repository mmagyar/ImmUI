 # ImmUI
 ### Functional, pure ui system


 ## Design principles
 
 #### No hard preconception in the usage.
 Although a basic model of operation is provided with examples, there is nothing stopping from the user to create their on concepts, creating new interaction models, or using it in immediate mode (recreation the graph on every render call) 
 
 #### Dependency free
 The core library does not (and should not) require any dependencies, external libraries.
 
 This means that this UI library can be used anywhere where you can run scala; JVM, JS, Native
 
 #### Nothing is tied to the concept of pixels
 The widget graph can be rendered at any resolution without artifacts, or even rendered into non-raster formats, such as SVG or HTML. This also makes it possible, and easy to interface with existing UI libraries
 
 #### Modularity
 The widgets and graphical primitives are in no way tied to any renderer or input method.
 Both the renderer and the input handler are totally separate concepts.
 
 This makes it possible to change graphical and input backend without ANY change in the UI code.
 
 #### Immutability
 Every change to a widget results in a new object.
 All widgets and graphical elements are fully immutable.
 
 This makes it possible, and easy to copy,save or restore state, and to process the graph on multiple threads. It also makes it easy to create meaningful diffs of changes, making it easy to optimize rendering and storing changes.
 
 #### Simple effect graph
 There are no events, every object can act on itself, and consequently all the elements below, but never above. A button should never be able to reach it's parent in any direct or indirect (global events) way. The proper solution is that a parent group should observe it's children.
 
 This can be done in one of two ways:
 
  - The child sets a flag on itself and the parent observes that. It can work, since changes upon behaviour are effected from the bottom of the tree to the top, so when the Behaviour is executed on the parent, all the children have already been processed by their Behaviour. In this case, in the parents Behaviour, the flag on it's children can be reset.
  - Watch the events directly. When a Behaviour function happens, it will receive all the affected elements, so the parent will always know if a children button has been pressed.
  
  It is also important to note, that the Behaviour will receive a Tracker object, that contains all the affected elements including parents, This can make it easier for widgets to act on itself, according to it's parents, but there is no danger of overly complex interactions, since there is no way to change it's parent in any way.  
  
#### Laziness
 Most calculated properties of elements should be lazy, since we don't always need all of the properties throughout the processing of object.
 
 
 #### No state management
 The library is completely stateless, every mutation must be handled outside of the library.
 
 #### No globals
 You can have as many widget graphs as you need. It is also to effortless to combine them at any time.
