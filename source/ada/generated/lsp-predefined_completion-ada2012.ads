--  Automatically generated, do not edit.

pragma Style_Checks (Off);

package LSP.Predefined_Completion.Ada2012 is

   Db : constant String := "{" & ASCII.LF
   & """PREDEFINED_ADA"": {" & ASCII.LF
   & """ASPECT"": [" & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Abstract_State: 1c.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Abstract_State""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Machine address of an entity. See 13.3.""," & ASCII.LF
   & """_id"": ""2/3""," & ASCII.LF
   & """_name"": ""Address""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Alignment of an object. See 13.3.""," & ASCII.LF
   & """_id"": ""3/3""," & ASCII.LF
   & """_name"": ""Alignment""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Alignment of a subtype. See 13.3.""," & ASCII.LF
   & """_id"": ""4/3""," & ASCII.LF
   & """_name"": ""Alignment""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""All indirect or dispatching remote subprogram calls and all\ndirect remote subprogram calls should use the Partition\nCommunication Subsystem. See E.2.3.""," & ASCII.LF
   & """_id"": ""5/4""," & ASCII.LF
   & """_name"": ""All_Calls_Remote""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""There are three forms of this aspect (where ID is an identifier, and\nARG is a general expression), corresponding to *note pragma Annotate:\n2a.\n\n`Annotate => ID'\nEquivalent to `pragma Annotate (ID, Entity => Name);'\n\n`Annotate => (ID)'\nEquivalent to `pragma Annotate (ID, Entity => Name);'\n\n`Annotate => (ID ,ID {, ARG})'\nEquivalent to `pragma Annotate (ID, ID {, ARG}, Entity => Name);'""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Annotate""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Async_Readers: 31.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Async_Readers""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Async_Writers: 34.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Async_Writers""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Remote procedure calls are asynchronous; the caller continues\nwithout waiting for the call to return. See E.4.1.""," & ASCII.LF
   & """_id"": ""6/3""," & ASCII.LF
   & """_name"": ""Asynchronous""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Declare that a type, object, or component is atomic. See C.6.""," & ASCII.LF
   & """_id"": ""7/3""," & ASCII.LF
   & """_name"": ""Atomic""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Declare that the components of an array type or object are\natomic. See C.6.""," & ASCII.LF
   & """_id"": ""8/3""," & ASCII.LF
   & """_name"": ""Atomic_Components""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Protected procedure is attached to an interrupt. See C.3.1.""," & ASCII.LF
   & """_id"": ""9/3""," & ASCII.LF
   & """_name"": ""Attach_Handler""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Order of bit numbering in a record_representation_clause. See""," & ASCII.LF
   & """_id"": ""10/3""," & ASCII.LF
   & """_name"": ""Bit_Order""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Processor on which a given task should run. See D.16.""," & ASCII.LF
   & """_id"": ""15/3""," & ASCII.LF
   & """_name"": ""CPU""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Internal representation of enumeration literals. Specified by\nan enumeration_representation_clause, not by an\naspect_specification. See 13.4.""," & ASCII.LF
   & """_id"": ""11/3""," & ASCII.LF
   & """_name"": ""Coding""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Size in bits of a component of an array type. See 13.3.""," & ASCII.LF
   & """_id"": ""12/3""," & ASCII.LF
   & """_name"": ""Component_Size""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Constant_After_Elaboration:\n45.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Constant_After_Elaboration""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Defines function(s) to implement user-defined\nindexed_components. See 4.1.6.""," & ASCII.LF
   & """_id"": ""13/3""," & ASCII.LF
   & """_name"": ""Constant_Indexing""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Contract_Cases: 47, the\nsequence of clauses being enclosed in parentheses so that syntactically\nit is an aggregate.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Contract_Cases""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Calling convention or other convention used for interfacing to\nother languages. See B.1.""," & ASCII.LF
   & """_id"": ""14/3""," & ASCII.LF
   & """_name"": ""Convention""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Default value for the components of an array-of-scalar\nsubtype. See 3.6.""," & ASCII.LF
   & """_id"": ""16/3""," & ASCII.LF
   & """_name"": ""Default_Component_Value""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Default_Initial_Condition: 51.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Default_Initial_Condition""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Default iterator to be used in for loops. See 5.5.1.""," & ASCII.LF
   & """_id"": ""17/3""," & ASCII.LF
   & """_name"": ""Default_Iterator""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Default storage pool for a generic instance. See 13.11.3.""," & ASCII.LF
   & """_id"": ""18/3""," & ASCII.LF
   & """_name"": ""Default_Storage_Pool""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Default value for a scalar subtype. See 3.5.\n\n19.1/4 Discard_Names\nRequests a reduction in storage for names associated with an\nentity. See C.5.""," & ASCII.LF
   & """_id"": ""19/3""," & ASCII.LF
   & """_name"": ""Default_Value""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Depends: 56.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Depends""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Dimension' aspect is used to specify the dimensions of a given\nsubtype of a dimensioned numeric type. The aspect also specifies a\nsymbol used when doing formatted output of dimensioned quantities. The\nsyntax is:\n\nwith Dimension =>\n([Symbol =>] SYMBOL, DIMENSION_VALUE {, DIMENSION_Value})\n\nSYMBOL ::= STRING_LITERAL | CHARACTER_LITERAL\n\nDIMENSION_VALUE ::=\nRATIONAL\n| others               => RATIONAL\n| DISCRETE_CHOICE_LIST => RATIONAL\n\nRATIONAL ::= [-] NUMERIC_LITERAL [/ NUMERIC_LITERAL]\n\nThis aspect can only be applied to a subtype whose parent type has a\n`Dimension_System' aspect. The aspect must specify values for all\ndimensions of the system. The rational values are the powers of the\ncorresponding dimensions that are used by the compiler to verify that\nphysical (numeric) computations are dimensionally consistent. For\nexample, the computation of a force must result in dimensions (L => 1,\nM => 1, T => -2).  For further examples of the usage of this aspect,\nsee package `System.Dim.Mks'.  Note that when the dimensioned type is\nan integer type, then any dimension value must be an integer literal.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Dimension""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Dimension_System' aspect is used to define a system of dimensions\nthat will be used in subsequent subtype declarations with `Dimension'\naspects that reference this system. The syntax is:\n\nwith Dimension_System => (DIMENSION {, DIMENSION});\n\nDIMENSION ::= ([Unit_Name   =>] IDENTIFIER,\n[Unit_Symbol =>] SYMBOL,\n[Dim_Symbol  =>] SYMBOL)\n\nSYMBOL ::= CHARACTER_LITERAL | STRING_LITERAL\n\nThis aspect is applied to a type, which must be a numeric derived type\n(typically a floating-point type), that will represent values within\nthe dimension system. Each `DIMENSION' corresponds to one particular\ndimension. A maximum of 7 dimensions may be specified. `Unit_Name' is\nthe name of the dimension (for example `Meter'). `Unit_Symbol' is the\nshorthand used for quantities of this dimension (for example `m' for\n`Meter').  `Dim_Symbol' gives the identification within the dimension\nsystem (typically this is a single letter, e.g. `L' standing for length\nfor unit name `Meter').  The `Unit_Symbol' is used in formatted output\nof dimensioned quantities.  The `Dim_Symbol' is used in error messages\nwhen numeric operations have inconsistent dimensions.\n\nGNAT provides the standard definition of the International MKS system in\nthe run-time package `System.Dim.Mks'. You can easily define similar\npackages for cgs units or British units, and define conversion factors\nbetween values in different systems. The MKS system is characterized by\nthe following aspect:\n\ntype Mks_Type is new Long_Long_Float with\nDimension_System => (\n(Unit_Name => Meter,    Unit_Symbol => 'm',   Dim_Symbol => 'L'),\n(Unit_Name => Kilogram, Unit_Symbol => \""kg\"",  Dim_Symbol => 'M'),\n(Unit_Name => Second,   Unit_Symbol => 's',   Dim_Symbol => 'T'),\n(Unit_Name => Ampere,   Unit_Symbol => 'A',   Dim_Symbol => 'I'),\n(Unit_Name => Kelvin,   Unit_Symbol => 'K',   Dim_Symbol => '@'),\n(Unit_Name => Mole,     Unit_Symbol => \""mol\"", Dim_Symbol => 'N'),\n(Unit_Name => Candela,  Unit_Symbol => \""cd\"",  Dim_Symbol => 'J'));\n\nNote that in the above type definition, we use the `at' symbol (`@') to\nrepresent a theta character (avoiding the use of extended Latin-1\ncharacters in this context).\n\nSee section 'Performing Dimensionality Analysis in GNAT' in the GNAT\nUsers Guide for detailed examples of use of the dimension system.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Dimension_System""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The aspect  `Disable_Controlled' is defined for controlled record\ntypes. If active, this aspect causes suppression of all related calls\nto `Initialize', `Adjust', and `Finalize'. The intended use is for\nconditional compilation, where for example you might want a record to\nbe controlled or not depending on whether some run-time check is\nenabled or suppressed.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Disable_Controlled""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Domain (group of processors) on which a given task should run.\nSee D.16.1.""," & ASCII.LF
   & """_id"": ""20/3""," & ASCII.LF
   & """_name"": ""Dispatching_Domain""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Condition that must hold true for objects of a given subtype;\nthe subtype is not static. See 3.2.4.""," & ASCII.LF
   & """_id"": ""21/3""," & ASCII.LF
   & """_name"": ""Dynamic_Predicate""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Effective_Reads: 5c.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Effective_Reads""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Effective_Writes: 5e.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Effective_Writes""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""A given package must have a body, and that body is elaborated\nimmediately after the declaration. See 10.2.1.\n\n22.1/4 Exclusive_Functions\nSpecifies mutual exclusion behavior of protected functions in\na protected type. See 9.5.1.""," & ASCII.LF
   & """_id"": ""22/3""," & ASCII.LF
   & """_name"": ""Elaborate_Body""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Entity is exported to another language. See B.1.""," & ASCII.LF
   & """_id"": ""23/3""," & ASCII.LF
   & """_name"": ""Export""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Extensions_Visible: 6a.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Extensions_Visible""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Name used to identify an imported or exported entity. See\nB.1.""," & ASCII.LF
   & """_id"": ""24/3""," & ASCII.LF
   & """_name"": ""External_Name""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Unique identifier for a tagged type in streams. See 13.3.""," & ASCII.LF
   & """_id"": ""25/3""," & ASCII.LF
   & """_name"": ""External_Tag""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Favor_Top_Level: 6f.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Favor_Top_Level""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Ghost: 72.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Ghost""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Global: 74.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Global""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Mechanism for user-defined implicit .all. See 4.1.5.""," & ASCII.LF
   & """_id"": ""26/3""," & ASCII.LF
   & """_name"": ""Implicit_Dereference""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Entity is imported from another language. See B.1.""," & ASCII.LF
   & """_id"": ""27/3""," & ASCII.LF
   & """_name"": ""Import""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Declare that a type, object, or component is independently\naddressable. See C.6.""," & ASCII.LF
   & """_id"": ""28/3""," & ASCII.LF
   & """_name"": ""Independent""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Declare that the components of an array or record type, or an\narray object, are independently addressable. See C.6.""," & ASCII.LF
   & """_id"": ""29/3""," & ASCII.LF
   & """_name"": ""Independent_Components""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Initial_Condition: 82.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Initial_Condition""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Initializes: 84.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Initializes""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For efficiency, Inline calls are requested for a subprogram.\nSee 6.3.2.""," & ASCII.LF
   & """_id"": ""30/3""," & ASCII.LF
   & """_name"": ""Inline""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Inline_Always: 87.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Inline_Always""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Function to read a value from a stream for a given type,\nincluding any bounds and discriminants. See 13.13.2.\n\n31.1/4 Input'Class\nFunction to read a value from a stream for a the class-wide\ntype associated with a given type, including any bounds and\ndiscriminants. See 13.13.2.""," & ASCII.LF
   & """_id"": ""31/3""," & ASCII.LF
   & """_name"": ""Input""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Protected procedure may be attached to interrupts. See C.3.1.""," & ASCII.LF
   & """_id"": ""32/3""," & ASCII.LF
   & """_name"": ""Interrupt_Handler""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Priority of a task object or type, or priority of a protected\nobject or type; the priority is in the interrupt range. See\nD.1.""," & ASCII.LF
   & """_id"": ""33/3""," & ASCII.LF
   & """_name"": ""Interrupt_Priority""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Invariant: 8e. It is a\nsynonym for the language defined aspect `Type_Invariant' except that it\nis separately controllable using pragma `Assertion_Policy'.\n\n3.22 Aspect Invariant'Class\n\nThis aspect is equivalent to *note pragma Type_Invariant_Class: 106. It\nis a synonym for the language defined aspect `Type_Invariant'Class'\nexcept that it is separately controllable using pragma\n`Assertion_Policy'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Invariant""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect provides a light-weight mechanism for loops and quantified\nexpressions over container types, without the overhead imposed by the\ntampering checks of standard Ada 2012 iterators. The value of the\naspect is an aggregate with six named components, of which the last\nthree are optional: `First', `Next', `Has_Element', `Element', `Last',\nand `Previous'.  When only the first three components are specified,\nonly the `for .. in' form of iteration over cursors is available. When\n`Element' is specified, both this form and the `for .. of' form of\niteration over elements are available. If the last two components are\nspecified, reverse iterations over the container can be specified\n(analogous to what can be done over predefined containers that support\nthe `Reverse_Iterator' interface).  The following is a typical example\nof use:\n\ntype List is private with\nIterable => (First        => First_Cursor,\nNext         => Advance,\nHas_Element  => Cursor_Has_Element,\n[Element      => Get_Element]);\n\n* The value denoted by `First' must denote a primitive operation of\nthe container type that returns a `Cursor', which must a be a type\ndeclared in the container package or visible from it. For example:\n\nfunction First_Cursor (Cont : Container) return Cursor;\n\n* The value of `Next' is a primitive operation of the container type\nthat takes both a container and a cursor and yields a cursor. For\nexample:\n\nfunction Advance (Cont : Container; Position : Cursor) return Cursor;\n\n* The value of `Has_Element' is a primitive operation of the\ncontainer type that takes both a container and a cursor and yields\na boolean. For example:\n\nfunction Cursor_Has_Element (Cont : Container; Position : Cursor) return Boolean;\n\n* The value of `Element' is a primitive operation of the container\ntype that takes both a container and a cursor and yields an\n`Element_Type', which must be a type declared in the container\npackage or visible from it. For example:\n\nfunction Get_Element (Cont : Container; Position : Cursor) return Element_Type;\n\nThis aspect is used in the GNAT-defined formal container packages.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Iterable""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Element type to be used for user-defined iterators. See""," & ASCII.LF
   & """_id"": ""34/3""," & ASCII.LF
   & """_name"": ""Iterator_Element""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Layout of record components. Specified by a\nrecord_representation_clause, not by an aspect_specification.\nSee 13.5.1.""," & ASCII.LF
   & """_id"": ""35/3""," & ASCII.LF
   & """_name"": ""Layout""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Linker symbol used to identify an imported or exported entity.\nSee B.1.""," & ASCII.LF
   & """_id"": ""36/3""," & ASCII.LF
   & """_name"": ""Link_Name""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Linker_Section: 96.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Linker_Section""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Lock_Free: 98.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Lock_Free""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Radix (2 or 10) that is used to represent a decimal fixed\npoint type. See F.1.""," & ASCII.LF
   & """_id"": ""37/3""," & ASCII.LF
   & """_name"": ""Machine_Radix""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Max_Queue_Length: a0.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Max_Queue_Length""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma No_Caching: a2.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Caching""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma No_Elaboration_Code_All: a6.\nfor a program unit.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Elaboration_Code_All""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma No_Inline: a9.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Inline""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""A procedure will not return normally. See 6.5.1.""," & ASCII.LF
   & """_id"": ""38/3""," & ASCII.LF
   & """_name"": ""No_Return""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma No_Tagged_Streams: ac. with an\nargument specifying a root tagged type (thus this aspect can only be\napplied to such a type).""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Tagged_Streams""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note attribute Object_Size: 147.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Object_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Obsolescent: af. Note that the\nevaluation of this aspect happens at the point of occurrence, it is not\ndelayed until the freeze point.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Obsolescent""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Procedure to write a value to a stream for a given type,\nincluding any bounds and discriminants. See 13.13.2.\n\n39.1/4 Output'Class\nProcedure to write a value to a stream for a the class-wide\ntype associated with a given type, including any bounds and\ndiscriminants. See 13.13.2.""," & ASCII.LF
   & """_id"": ""39/3""," & ASCII.LF
   & """_name"": ""Output""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Minimize storage when laying out records and arrays. See""," & ASCII.LF
   & """_id"": ""40/3""," & ASCII.LF
   & """_name"": ""Pack""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Part_Of: b7.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Part_Of""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Persistent_BSS: ba.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Persistent_BSS""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Postcondition; a condition that must hold true after a call.\nSee 6.1.1.""," & ASCII.LF
   & """_id"": ""41/3""," & ASCII.LF
   & """_name"": ""Post""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Postcondition inherited on type derivation. See 6.1.1.""," & ASCII.LF
   & """_id"": ""42/3""," & ASCII.LF
   & """_name"": ""Post'Class""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Precondition; a condition that must hold true before a call.\nSee 6.1.1.""," & ASCII.LF
   & """_id"": ""43/3""," & ASCII.LF
   & """_name"": ""Pre""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Precondition inherited on type derivation. See 6.1.1.\n\n44.1/4 Predicate_Failure\nAction to be performed when a predicate check fails. See""," & ASCII.LF
   & """_id"": ""44/3""," & ASCII.LF
   & """_name"": ""Pre'Class""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Predicate: c2. It is thus\nsimilar to the language defined aspects `Dynamic_Predicate' and\n`Static_Predicate' except that whether the resulting predicate is\nstatic or dynamic is controlled by the form of the expression. It is\nalso separately controllable using pragma `Assertion_Policy'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Predicate""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Code execution during elaboration is avoided for a given\npackage. See 10.2.1.""," & ASCII.LF
   & """_id"": ""45/3""," & ASCII.LF
   & """_name"": ""Preelaborate""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Priority of a task object or type, or priority of a protected\nobject or type; the priority is not in the interrupt range.\nSee D.1.""," & ASCII.LF
   & """_id"": ""46/3""," & ASCII.LF
   & """_name"": ""Priority""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Side effects are avoided in the subprograms of a given\npackage. See 10.2.1.""," & ASCII.LF
   & """_id"": ""47/3""," & ASCII.LF
   & """_name"": ""Pure""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Pure_Function: ce.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Pure_Function""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Procedure to read a value from a stream for a given type. See\n\n\n48.1/4 Read'Class\nProcedure to read a value from a stream for the class-wide\ntype associated with a given type. See 13.13.2.""," & ASCII.LF
   & """_id"": ""48/3""," & ASCII.LF
   & """_name"": ""Read""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""layout\n\nSee Layout. See 13.5.1.""," & ASCII.LF
   & """_id"": ""49/3""," & ASCII.LF
   & """_name"": ""Record""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Refined_Depends: d2.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Refined_Depends""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Refined_Global: d4.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Refined_Global""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Refined_Post: d6.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Refined_Post""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Refined_State: d8.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Refined_State""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Task parameter used in Earliest Deadline First Dispatching.\nSee D.2.6.""," & ASCII.LF
   & """_id"": ""50/3""," & ASCII.LF
   & """_name"": ""Relative_Deadline""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Remote_Access_Type: dc.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Remote_Access_Type""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Subprograms in a given package may be used in remote procedure\ncalls. See E.2.3.""," & ASCII.LF
   & """_id"": ""51/3""," & ASCII.LF
   & """_name"": ""Remote_Call_Interface""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Types in a given package may be used in remote procedure\ncalls. See E.2.2.""," & ASCII.LF
   & """_id"": ""52/3""," & ASCII.LF
   & """_name"": ""Remote_Types""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma SPARK_Mode: ef. and may be\nspecified for either or both of the specification and body of a\nsubprogram or package.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""SPARK_Mode""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to a *note attribute Scalar_Storage_Order:\n154.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Scalar_Storage_Order""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Secondary_Stack_Size: e1.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Secondary_Stack_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Shared: e4.  and is\nthus a synonym for aspect `Atomic'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Shared""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""A given package is used to represent shared memory in a\ndistributed system. See E.2.1.""," & ASCII.LF
   & """_id"": ""53/3""," & ASCII.LF
   & """_name"": ""Shared_Passive""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note attribute Simple_Storage_Pool: e9.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Simple_Storage_Pool""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma\nSimple_Storage_Pool_Type: e7.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Simple_Storage_Pool_Type""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Size in bits of an object. See 13.3.""," & ASCII.LF
   & """_id"": ""54/3""," & ASCII.LF
   & """_name"": ""Size""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Size in bits of a subtype. See 13.3.""," & ASCII.LF
   & """_id"": ""55/3""," & ASCII.LF
   & """_name"": ""Size""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Scale factor for a fixed point type. See 3.5.10.""," & ASCII.LF
   & """_id"": ""56/3""," & ASCII.LF
   & """_name"": ""Small""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Condition that must hold true for objects of a given subtype;\nthe subtype may be static. See 3.2.4.""," & ASCII.LF
   & """_id"": ""57/3""," & ASCII.LF
   & """_name"": ""Static_Predicate""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Pool of memory from which new will allocate for a given access\ntype. See 13.11.""," & ASCII.LF
   & """_id"": ""58/3""," & ASCII.LF
   & """_name"": ""Storage_Pool""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Sets memory size for allocations for an access type. See""," & ASCII.LF
   & """_id"": ""59/3""," & ASCII.LF
   & """_name"": ""Storage_Size""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Size in storage elements reserved for a task type or single\ntask object. See 13.3.""," & ASCII.LF
   & """_id"": ""60/3""," & ASCII.LF
   & """_name"": ""Storage_Size""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Size in bits used to represent elementary objects in a stream.\nSee 13.13.2.""," & ASCII.LF
   & """_id"": ""61/3""," & ASCII.LF
   & """_name"": ""Stream_Size""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Suppress_Debug_Info:\nf7.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Suppress_Debug_Info""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma\nSuppress_Initialization: fb.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Suppress_Initialization""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Defines whether a given primitive operation of a synchronized\ninterface must be implemented by an entry or protected\nprocedure. See 9.5.""," & ASCII.LF
   & """_id"": ""62/3""," & ASCII.LF
   & """_name"": ""Synchronization""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Test_Case: fe.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Test_Case""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""subclause summarizes the definitions given elsewhere of the\nlanguage-defined aspects. Aspects are properties of entities that can be\nspecified by the Ada program; unless otherwise specified below, aspects can be\nspecified using an aspect_specification.""," & ASCII.LF
   & """_id"": ""1/3""," & ASCII.LF
   & """_name"": ""This""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Thread_Local_Storage:\n100.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Thread_Local_Storage""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""A condition that must hold true for all objects of a type. See""," & ASCII.LF
   & """_id"": ""63/3""," & ASCII.LF
   & """_name"": ""Type_Invariant""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""A condition that must hold true for all objects in a class of\ntypes. See 7.3.2.""," & ASCII.LF
   & """_id"": ""64/3""," & ASCII.LF
   & """_name"": ""Type_Invariant'Class""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Type is used to interface to a C union type. See B.3.3.""," & ASCII.LF
   & """_id"": ""65/3""," & ASCII.LF
   & """_name"": ""Unchecked_Union""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Universal_Aliasing:\n10a.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Universal_Aliasing""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note pragma Universal_Data: 10c.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Universal_Data""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Unmodified: 10f.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unmodified""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Unreferenced: 110.\nNote that in the case of formal parameters, it is not permitted to have\naspects for a formal parameter, so in this case the pragma form must be\nused.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unreferenced""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Unreferenced_Objects:\n112.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unreferenced_Objects""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to *note attribute Value_Size: 163.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Value_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Defines function(s) to implement user-defined\nindexed_components. See 4.1.6.""," & ASCII.LF
   & """_id"": ""66/3""," & ASCII.LF
   & """_name"": ""Variable_Indexing""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Declare that a type, object, or component is volatile. See\nC.6.""," & ASCII.LF
   & """_id"": ""67/3""," & ASCII.LF
   & """_name"": ""Volatile""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Declare that the components of an array type or object are\nvolatile. See C.6.""," & ASCII.LF
   & """_id"": ""68/3""," & ASCII.LF
   & """_name"": ""Volatile_Components""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Volatile_Full_Access:\n11d.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Volatile_Full_Access""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This boolean aspect is equivalent to *note pragma Volatile_Function:\n11f.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Volatile_Function""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This aspect is equivalent to the two argument form of *note pragma\nWarnings: 121, where the first argument is `ON' or `OFF' and the second\nargument is the entity.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Warnings""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Procedure to write a value to a stream for a given type. See\n\n\n69.1/4 Write'Class\nProcedure to write a value to a stream for a the class-wide\ntype associated with a given type. See 13.13.2.""," & ASCII.LF
   & """_id"": ""69/3""," & ASCII.LF
   & """_name"": ""Write""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}" & ASCII.LF
   & "]," & ASCII.LF
   & """ATTRIBUTE"": [" & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`Standard'Abort_Signal' (`Standard' is the only allowed prefix)\nprovides the entity for the special exception used to signal task abort\nor asynchronous transfer of control.  Normally this attribute should\nonly be used in the tasking runtime (it is highly peculiar, and\ncompletely outside the normal semantics of Ada, for a user program to\nintercept the abort exception).""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Abort_Signal""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix P that denotes a subprogram:\n\n\nP'Access yields an access value that designates the subprogram\ndenoted by P. The type of P'Access is an access-to-subprogram\ntype (S), as determined by the expected type. See 3.10.2.""," & ASCII.LF
   & """_id"": ""2""," & ASCII.LF
   & """_name"": ""Access""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes an aliased view of an object:\n\n\nX'Access yields an access value that designates the object\ndenoted by X. The type of X'Access is an access-to-object\ntype, as determined by the expected type. The expected type\nshall be a general access type. See 3.10.2.""," & ASCII.LF
   & """_id"": ""4""," & ASCII.LF
   & """_name"": ""Access""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes an object, program unit, or label:\n\n\nDenotes the address of the first of the storage elements\nallocated to X. For a program unit or label, this value refers\nto the machine code associated with the corresponding body or\nstatement. The value of this attribute is of type\nSystem.Address. See 13.3.""," & ASCII.LF
   & """_id"": ""6/1""," & ASCII.LF
   & """_name"": ""Address""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`Standard'Address_Size' (`Standard' is the only allowed prefix) is a\nstatic constant giving the number of bits in an `Address'. It is the\nsame value as System.Address'Size, but has the advantage of being\nstatic, while a direct reference to System.Address'Size is nonstatic\nbecause Address is a private type.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Address_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Adjacent denotes a function with the following\nspecification:\n\n\nfunction S'Adjacent (X, Towards : T)\nreturn T\n\n\nIf Towards = X, the function yields X; otherwise, it yields\nthe machine number of the type T adjacent to X in the\ndirection of Towards, if that machine number exists. If the\nresult would be outside the base range of S, Constraint_Error\nis raised. When T'Signed_Zeros is True, a zero result has the\nsign of X. When Towards is zero, its sign has no bearing on\nthe result. See A.5.3.""," & ASCII.LF
   & """_id"": ""8""," & ASCII.LF
   & """_name"": ""Adjacent""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every fixed point subtype S:\n\n\nS'Aft yields the number of decimal digits needed after the\ndecimal point to accommodate the delta of the subtype S,\nunless the delta of the subtype S is greater than 0.1, in\nwhich case the attribute yields the value one. (S'Aft is the\nsmallest positive integer N for which (10**N)*S'Delta is\ngreater than or equal to one.) The value of this attribute is\nof the type universal_integer. See 3.5.10.""," & ASCII.LF
   & """_id"": ""12""," & ASCII.LF
   & """_name"": ""Aft""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S:\n\n\nThe value of this attribute is of type universal_integer, and\nnonnegative.\n\n\nFor an object X of subtype S, if S'Alignment is not zero, then\nX'Alignment is a nonzero integral multiple of S'Alignment\nunless specified otherwise by a representation item. See""," & ASCII.LF
   & """_id"": ""13.1/2""," & ASCII.LF
   & """_name"": ""Alignment""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes an object:\n\n\nThe value of this attribute is of type universal_integer, and\nnonnegative; zero means that the object is not necessarily\naligned on a storage element boundary. If X'Alignment is not\nzero, then X is aligned on a storage unit boundary and\nX'Address is an integral multiple of X'Alignment (that is, the\nAddress modulo the Alignment is zero).""," & ASCII.LF
   & """_id"": ""14/1""," & ASCII.LF
   & """_name"": ""Alignment""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Asm_Input' attribute denotes a function that takes two parameters.\nThe first is a string, the second is an expression of the type\ndesignated by the prefix.  The first (string) argument is required to\nbe a static expression, and is the constraint for the parameter, (e.g.,\nwhat kind of register is required).  The second argument is the value\nto be used as the input argument.  The possible values for the constant\nare the same as those used in the RTL, and are dependent on the\nconfiguration file used to built the GCC back end.  *note Machine Code\nInsertions: 16c.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Asm_Input""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Asm_Output' attribute denotes a function that takes two\nparameters.  The first is a string, the second is the name of a variable\nof the type designated by the attribute prefix.  The first (string)\nargument is required to be a static expression and designates the\nconstraint for the parameter (e.g., what kind of register is required).\nThe second argument is the variable to be updated with the result.  The\npossible values for constraint are the same as those used in the RTL,\nand are dependent on the configuration file used to build the GCC back\nend.  If there are no output operands, then this argument may either be\nomitted, or explicitly given as `No_Output_Operands'.  *note Machine\nCode Insertions: 16c.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Asm_Output""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The prefix of the `Atomic_Always_Lock_Free' attribute is a type.  The\nresult is a Boolean value which is True if the type has discriminants,\nand False otherwise.  The result indicate whether atomic operations are\nsupported by the target for the given type.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Atomic_Always_Lock_Free""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Base denotes an unconstrained subtype of the type of S. This\nunconstrained subtype is called the base subtype of the type.\nSee 3.5.""," & ASCII.LF
   & """_id"": ""17""," & ASCII.LF
   & """_name"": ""Base""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`obj'Bit', where `obj' is any object, yields the bit offset within the\nstorage unit (byte) that contains the first bit of storage allocated\nfor the object.  The value of this attribute is of the type\n`universal_integer', and is always a non-negative number not exceeding\nthe value of `System.Storage_Unit'.\n\nFor an object that is a variable or a constant allocated in a register,\nthe value is zero.  (The use of this attribute does not force the\nallocation of a variable to memory).\n\nFor an object that is a formal parameter, this attribute applies to\neither the matching actual parameter or to a copy of the matching\nactual parameter.\n\nFor an access object the value is zero.  Note that `obj.all'Bit' is\nsubject to an `Access_Check' for the designated object.  Similarly for\na record component `X.C'Bit' is subject to a discriminant check and\n`X(I).Bit' and `X(I1..I2)'Bit' are subject to index checks.\n\nThis attribute is designed to be compatible with the DEC Ada 83\ndefinition and implementation of the `Bit' attribute.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Bit""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every specific record subtype S:\n\n\nDenotes the bit ordering for the type of S. The value of this\nattribute is of type System.Bit_Order. See 13.5.3.""," & ASCII.LF
   & """_id"": ""19""," & ASCII.LF
   & """_name"": ""Bit_Order""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`R.C'Bit_Position', where `R' is a record object and `C' is one of the\nfields of the record type, yields the bit offset within the record\ncontains the first bit of storage allocated for the object.  The value\nof this attribute is of the type `universal_integer'.  The value\ndepends only on the field `C' and is independent of the alignment of\nthe containing record `R'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Bit_Position""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix P that statically denotes a program unit:\n\n\nYields a value of the predefined type String that identifies\nthe version of the compilation unit that contains the body\n(but not any subunits) of the program unit. See E.3.""," & ASCII.LF
   & """_id"": ""21/1""," & ASCII.LF
   & """_name"": ""Body_Version""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix T that is of a task type (after any implicit\ndereference):\n\n\nYields the value True when the task denoted by T is callable,\nand False otherwise; See 9.9.""," & ASCII.LF
   & """_id"": ""23""," & ASCII.LF
   & """_name"": ""Callable""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix E that denotes an entry_declaration:\n\n\nYields a value of the type Task_Id that identifies the task\nwhose call is now being serviced. Use of this attribute is\nallowed only inside an accept_statement, or entry_body after\nthe entry_barrier, corresponding to the entry_declaration\ndenoted by E. See C.7.1.""," & ASCII.LF
   & """_id"": ""25""," & ASCII.LF
   & """_name"": ""Caller""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Ceiling denotes a function with the following specification:\n\n\nfunction S'Ceiling (X : T)\nreturn T\n\n\nThe function yields the value Ceiling(X), i.e., the smallest\n(most negative) integral value greater than or equal to X.\nWhen X is zero, the result has the sign of X; a zero result\notherwise has a negative sign when S'Signed_Zeros is True. See\nA.5.3.""," & ASCII.LF
   & """_id"": ""27""," & ASCII.LF
   & """_name"": ""Ceiling""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a tagged type T (specific or\nclass-wide):\n\n\nS'Class denotes a subtype of the class-wide type (called\nT'Class in this International Standard) for the class rooted\nat T (or if S already denotes a class-wide subtype, then\nS'Class is the same as S).\n\n\nS'Class is unconstrained. However, if S is constrained, then\nthe values of S'Class are only those that when converted to\nthe type T belong to S. See 3.9.""," & ASCII.LF
   & """_id"": ""31""," & ASCII.LF
   & """_name"": ""Class""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of an untagged private type whose full\nview is tagged:\n\n\nDenotes the class-wide subtype corresponding to the full view\nof S. This attribute is allowed only from the beginning of the\nprivate part in which the full view is declared, until the\ndeclaration of the full view. After the full view, the Class\nattribute of the full view can be used. See 7.3.1.""," & ASCII.LF
   & """_id"": ""34""," & ASCII.LF
   & """_name"": ""Class""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S'Class of a class-wide type T'Class:\n\n\nS'Class'Input denotes a function with the following\nspecification:\n\n\nfunction S'Class'Input(\nStream : not null access Ada.Streams.Root_Stream_Type'Class)\nreturn T'Class\n\n\nFirst reads the external tag from Stream and determines the\ncorresponding internal tag (by calling\nTags.Descendant_Tag(String'Input(Stream), S'Tag) which might\nraise Tag_Error - see 3.9) and then dispatches to the\nsubprogram denoted by the Input attribute of the specific type\nidentified by the internal tag; returns that result. If the\nspecific type identified by the internal tag is abstract,\nConstraint_Error is raised. See 13.13.2.""," & ASCII.LF
   & """_id"": ""92""," & ASCII.LF
   & """_name"": ""Class'Input""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S'Class of a class-wide type T'Class:\n\n\nS'Class'Output denotes a procedure with the following\nspecification:\n\n\nprocedure S'Class'Output(\nStream : not null access Ada.Streams.Root_Stream_Type'Class;\nItem   : in T'Class)\n\n\nFirst writes the external tag of Item to Stream (by calling\nString'Output(Stream, Tags.External_Tag(Item'Tag)) - see 3.9)\nand then dispatches to the subprogram denoted by the Output\nattribute of the specific type identified by the tag.\nTag_Error is raised if the tag of Item identifies a type\ndeclared at an accessibility level deeper than that of S. See""," & ASCII.LF
   & """_id"": ""165""," & ASCII.LF
   & """_name"": ""Class'Output""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""procedure""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S'Class of a class-wide type T'Class:\n\n\nS'Class'Read denotes a procedure with the following\nspecification:\n\n\nprocedure S'Class'Read(\nStream : not null access Ada.Streams.Root_Stream_Type'Class;\nItem : out T'Class)\n\n\nDispatches to the subprogram denoted by the Read attribute of\nthe specific type identified by the tag of Item. See 13.13.2.""," & ASCII.LF
   & """_id"": ""191""," & ASCII.LF
   & """_name"": ""Class'Read""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""procedure""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S'Class of a class-wide type T'Class:\n\n\nS'Class'Write denotes a procedure with the following\nspecification:\n\n\nprocedure S'Class'Write(\nStream : not null access Ada.Streams.Root_Stream_Type'Class;\nItem   : in T'Class)\n\n\nDispatches to the subprogram denoted by the Write attribute of\nthe specific type identified by the tag of Item. See 13.13.2.""," & ASCII.LF
   & """_id"": ""282""," & ASCII.LF
   & """_name"": ""Class'Write""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""procedure""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `'Address' attribute may be applied to subprograms in Ada 95 and\nAda 2005, but the intended effect seems to be to provide an address\nvalue which can be used to call the subprogram by means of an address\nclause as in the following example:\n\nprocedure K is ...\n\nprocedure L;\nfor L'Address use K'Address;\npragma Import (Ada, L);\n\nA call to `L' is then expected to result in a call to `K'.  In Ada 83,\nwhere there were no access-to-subprogram values, this was a common\nwork-around for getting the effect of an indirect call.  GNAT\nimplements the above use of `Address' and the technique illustrated by\nthe example code works correctly.\n\nHowever, for some purposes, it is useful to have the address of the\nstart of the generated code for the subprogram.  On some architectures,\nthis is not necessarily the same as the `Address' value described above.\nFor example, the `Address' value may reference a subprogram descriptor\nrather than the subprogram itself.\n\nThe `'Code_Address' attribute, which can only be applied to subprogram\nentities, always returns the address of the start of the generated code\nof the specified subprogram, which may or may not be the same value as\nis returned by the corresponding `'Address' attribute.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Code_Address""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`Standard'Compiler_Version' (`Standard' is the only allowed prefix)\nyields a static string identifying the version of the compiler being\nused to compile the unit containing the attribute reference.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Compiler_Version""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes an array subtype or array object\n(after any implicit dereference):\n\n\nDenotes the size in bits of components of the type of X. The\nvalue of this attribute is of type universal_integer. See""," & ASCII.LF
   & """_id"": ""36/1""," & ASCII.LF
   & """_name"": ""Component_Size""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Compose denotes a function with the following specification:\n\n\nfunction S'Compose (Fraction : T;\nExponent : universal_integer)\nreturn T\n\n\nLet v be the value Fraction x T'Machine_Radix(Exponent-k),\nwhere k is the normalized exponent of Fraction. If v is a\nmachine number of the type T, or if |v| >= T'Model_Small, the\nfunction yields v; otherwise, it yields either one of the\nmachine numbers of the type T adjacent to v. Constraint_Error\nis optionally raised if v is outside the base range of S. A\nzero result has the sign of Fraction when S'Signed_Zeros is\nTrue. See A.5.3.""," & ASCII.LF
   & """_id"": ""38""," & ASCII.LF
   & """_name"": ""Compose""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""In addition to the usage of this attribute in the Ada RM, GNAT also\npermits the use of the `'Constrained' attribute in a generic template\nfor any type, including types without discriminants. The value of this\nattribute in the generic instance when applied to a scalar type or a\nrecord type without discriminants is always `True'. This usage is\ncompatible with older Ada compilers, including notably DEC Ada.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Constrained""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix A that is of a discriminated type (after any\nimplicit dereference):\n\n\nYields the value True if A denotes a constant, a value, a\ntagged object, or a constrained variable, and False otherwise.\nSee 3.7.2.""," & ASCII.LF
   & """_id"": ""42""," & ASCII.LF
   & """_name"": ""Constrained""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Copy_Sign denotes a function with the following\nspecification:\n\n\nfunction S'Copy_Sign (Value, Sign : T)\nreturn T\n\n\nIf the value of Value is nonzero, the function yields a result\nwhose magnitude is that of Value and whose sign is that of\nSign; otherwise, it yields the value zero. Constraint_Error is\noptionally raised if the result is outside the base range of\nS. A zero result has the sign of Sign when S'Signed_Zeros is\nTrue. See A.5.3.""," & ASCII.LF
   & """_id"": ""44""," & ASCII.LF
   & """_name"": ""Copy_Sign""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix E that denotes an entry of a task or protected\nunit:\n\n\nYields the number of calls presently queued on the entry E of\nthe current instance of the unit. The value of this attribute\nis of the type universal_integer. See 9.9.""," & ASCII.LF
   & """_id"": ""48""," & ASCII.LF
   & """_name"": ""Count""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`Standard'Default_Bit_Order' (`Standard' is the only permissible\nprefix), provides the value `System.Default_Bit_Order' as a `Pos' value\n(0 for `High_Order_First', 1 for `Low_Order_First').  This is used to\nconstruct the definition of `Default_Bit_Order' in package `System'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Default_Bit_Order""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`Standard'Default_Scalar_Storage_Order' (`Standard' is the only\npermissible prefix), provides the current value of the default scalar\nstorage order (as specified using pragma\n`Default_Scalar_Storage_Order', or equal to `Default_Bit_Order' if\nunspecified) as a `System.Bit_Order' value. This is a static attribute.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Default_Scalar_Storage_Order""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix S that denotes a formal indefinite subtype:\n\n\nS'Definite yields True if the actual subtype corresponding to\nS is definite; otherwise, it yields False. The value of this\nattribute is of the predefined type Boolean. See 12.5.1.""," & ASCII.LF
   & """_id"": ""50/1""," & ASCII.LF
   & """_name"": ""Definite""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every fixed point subtype S:\n\n\nS'Delta denotes the delta of the fixed point subtype S. The\nvalue of this attribute is of the type universal_real. See""," & ASCII.LF
   & """_id"": ""52""," & ASCII.LF
   & """_name"": ""Delta""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nYields the value True if every value expressible in the form\n+- mantissa x T'Machine_Radix(T'Machine_Emin)\nwhere mantissa is a nonzero T'Machine_Mantissa-digit fraction\nin the number base T'Machine_Radix, the first digit of which\nis zero, is a machine number (see 3.5.7) of the type T; yields\nthe value False otherwise. The value of this attribute is of\nthe predefined type Boolean. See A.5.3.""," & ASCII.LF
   & """_id"": ""54""," & ASCII.LF
   & """_name"": ""Denorm""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The attribute `typ'Deref(expr)' where `expr' is of type\n`System.Address' yields the variable of type `typ' that is located at\nthe given address. It is similar to `(totyp (expr).all)', where `totyp'\nis an unchecked conversion from address to a named access-to-`typ'\ntype, except that it yields a variable, so it can be used on the left\nside of an assignment.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Deref""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Nonstatic attribute `Descriptor_Size' returns the size in bits of the\ndescriptor allocated for a type.  The result is non-zero only for\nunconstrained array types and the returned value is of type universal\ninteger.  In GNAT, an array descriptor contains bounds information and\nis located immediately before the first element of the array.\n\ntype Unconstr_Array is array (Positive range <>) of Boolean;\nPut_Line (\""Descriptor size = \"" & Unconstr_Array'Descriptor_Size'Img);\n\nThe attribute takes into account any additional padding due to type\nalignment.  In the example above, the descriptor contains two values of\ntype `Positive' representing the low and high bound.  Since `Positive'\nhas a size of 31 bits and an alignment of 4, the descriptor size is `2\n* Positive'Size + 2' or 64 bits.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Descriptor_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every floating point subtype S:\n\n\nS'Digits denotes the requested decimal precision for the\nsubtype S. The value of this attribute is of the type\nuniversal_integer. See 3.5.8.""," & ASCII.LF
   & """_id"": ""56""," & ASCII.LF
   & """_name"": ""Digits""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every decimal fixed point subtype S:\n\n\nS'Digits denotes the digits of the decimal fixed point subtype\nS, which corresponds to the number of decimal digits that are\nrepresentable in objects of the subtype. The value of this\nattribute is of the type universal_integer. See 3.5.10.""," & ASCII.LF
   & """_id"": ""58""," & ASCII.LF
   & """_name"": ""Digits""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This attribute can only be applied to a program unit name.  It returns\nthe entity for the corresponding elaboration procedure for elaborating\nthe body of the referenced unit.  This is used in the main generated\nelaboration procedure by the binder and is not normally used in any\nother context.  However, there may be specialized situations in which it\nis useful to be able to call this elaboration procedure from Ada code,\ne.g., if it is necessary to do selective re-elaboration to fix some\nerror.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Elab_Body""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""procedure""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This attribute can only be applied to a program unit name.  It returns\nthe entity for the corresponding elaboration procedure for elaborating\nthe spec of the referenced unit.  This is used in the main generated\nelaboration procedure by the binder and is not normally used in any\nother context.  However, there may be specialized situations in which\nit is useful to be able to call this elaboration procedure from Ada\ncode, e.g., if it is necessary to do selective re-elaboration to fix\nsome error.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Elab_Spec""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""procedure""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This attribute can only be applied to a library level subprogram name\nand is only allowed in CodePeer mode. It returns the entity for the\ncorresponding elaboration procedure for elaborating the body of the\nreferenced subprogram unit. This is used in the main generated\nelaboration procedure by the binder in CodePeer mode only and is\nunrecognized otherwise.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Elab_Subp_Body""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The prefix of the `'Elaborated' attribute must be a unit name.  The\nvalue is a Boolean which indicates whether or not the given unit has\nbeen elaborated.  This attribute is primarily intended for internal use\nby the generated code for dynamic elaboration checking, but it can also\nbe used in user programs.  The value will always be True once\nelaboration of all units has been completed.  An exception is for units\nwhich need no elaboration, the value is always False for such units.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Elaborated""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Emax' attribute is provided for compatibility with Ada 83.  See\nthe Ada 83 reference manual for an exact description of the semantics of\nthis attribute.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Emax""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Enabled' attribute allows an application program to check at\ncompile time to see if the designated check is currently enabled. The\nprefix is a simple identifier, referencing any predefined check name\n(other than `All_Checks') or a check name introduced by pragma\nCheck_Name. If no argument is given for the attribute, the check is for\nthe general state of the check, if an argument is given, then it is an\nentity name, and the check indicates whether an `Suppress' or\n`Unsuppress' has been given naming the entity (if not, then the\nargument is ignored).\n\nNote that instantiations inherit the check status at the point of the\ninstantiation, so a useful idiom is to have a library package that\nintroduces a check name with `pragma Check_Name', and then contains\ngeneric packages or subprograms which use the `Enabled' attribute to\nsee if the check is enabled. A user of this package can then issue a\n`pragma Suppress' or `pragma Unsuppress' before instantiating the\npackage or subprogram, controlling whether the check will be present.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Enabled""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every enumeration subtype `S', `S'Enum_Rep' denotes a function with\nthe following spec:\n\nfunction S'Enum_Rep (Arg : S'Base) return <Universal_Integer>;\n\nIt is also allowable to apply `Enum_Rep' directly to an object of an\nenumeration type or to a non-overloaded enumeration literal.  In this\ncase `S'Enum_Rep' is equivalent to `typ'Enum_Rep(S)' where `typ' is the\ntype of the enumeration literal or object.\n\nThe function returns the representation value for the given enumeration\nvalue.  This will be equal to value of the `Pos' attribute in the\nabsence of an enumeration representation clause.  This is a static\nattribute (i.e.,:the result is static if the argument is static).\n\n`S'Enum_Rep' can also be used with integer types and objects, in which\ncase it simply returns the integer value.  The reason for this is to\nallow it to be used for `(<>)' discrete formal arguments in a generic\nunit that can be instantiated with either enumeration types or integer\ntypes.  Note that if `Enum_Rep' is used on a modular type whose upper\nbound exceeds the upper bound of the largest signed integer type, and\nthe argument is a variable, so that the universal integer calculation\nis done at run time, then the call to `Enum_Rep' may raise\n`Constraint_Error'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Enum_Rep""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every enumeration subtype `S', `S'Enum_Val' denotes a function with\nthe following spec:\n\nfunction S'Enum_Val (Arg : <Universal_Integer>) return S'Base;\n\nThe function returns the enumeration value whose representation matches\nthe argument, or raises Constraint_Error if no enumeration literal of\nthe type has the matching value.  This will be equal to value of the\n`Val' attribute in the absence of an enumeration representation clause.\nThis is a static attribute (i.e., the result is static if the argument\nis static).""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Enum_Val""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Epsilon' attribute is provided for compatibility with Ada 83.  See\nthe Ada 83 reference manual for an exact description of the semantics of\nthis attribute.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Epsilon""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Exponent denotes a function with the following\nspecification:\n\n\nfunction S'Exponent (X : T)\nreturn universal_integer\n\n\nThe function yields the normalized exponent of X. See A.5.3.""," & ASCII.LF
   & """_id"": ""60""," & ASCII.LF
   & """_name"": ""Exponent""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a tagged type T (specific or\nclass-wide):\n\n\nS'External_Tag denotes an external string representation for\nS'Tag; it is of the predefined type String. External_Tag may\nbe specified for a specific tagged type via an\nattribute_definition_clause; the expression of such a clause\nshall be static. The default external tag representation is\nimplementation defined. See 13.13.2. See 13.3.""," & ASCII.LF
   & """_id"": ""64""," & ASCII.LF
   & """_name"": ""External_Tag""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`Standard'Fast_Math' (`Standard' is the only allowed prefix) yields a\nstatic Boolean value that is True if pragma `Fast_Math' is active, and\nFalse otherwise.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Fast_Math""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The prefix of attribute `Finalization_Size' must be an object or a\nnon-class-wide type. This attribute returns the size of any hidden data\nreserved by the compiler to handle finalization-related actions. The\ntype of the attribute is `universal_integer'.\n\n`Finalization_Size' yields a value of zero for a type with no controlled\nparts, an object whose type has no controlled parts, or an object of a\nclass-wide type whose tag denotes a type with no controlled parts.\n\nNote that only heap-allocated objects contain finalization data.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Finalization_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix A that is of an array type (after any implicit\ndereference), or denotes a constrained array subtype:\n\n\nA'First denotes the lower bound of the first index range; its\ntype is the corresponding index type. See 3.6.2.""," & ASCII.LF
   & """_id"": ""66/1""," & ASCII.LF
   & """_name"": ""First""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'First denotes the lower bound of the range of S. The value\nof this attribute is of the type of S. See 3.5.""," & ASCII.LF
   & """_id"": ""68""," & ASCII.LF
   & """_name"": ""First""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix A that is of an array type (after any implicit\ndereference), or denotes a constrained array subtype:\n\n\nA'First(N) denotes the lower bound of the N-th index range;\nits type is the corresponding index type. See 3.6.2.""," & ASCII.LF
   & """_id"": ""70/1""," & ASCII.LF
   & """_name"": ""First(N)""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a component C of a composite, non-array object R:\n\n\nIf the nondefault bit ordering applies to the composite type,\nand if a component_clause specifies the placement of C,\ndenotes the value given for the first_bit of the\ncomponent_clause; otherwise, denotes the offset, from the\nstart of the first of the storage elements occupied by C, of\nthe first bit occupied by C. This offset is measured in bits.\nThe first bit of a storage element is numbered zero. The value\nof this attribute is of the type universal_integer. See""," & ASCII.LF
   & """_id"": ""72""," & ASCII.LF
   & """_name"": ""First_Bit""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every static discrete subtype S for which there exists at\nleast one value belonging to S that satisfies the predicates\nof S:\n\n\nS'First_Valid denotes the smallest value that belongs to S and\nsatisfies the predicates of S. The value of this attribute is\nof the type of S. See 3.5.5.""," & ASCII.LF
   & """_id"": ""73.1/4""," & ASCII.LF
   & """_name"": ""First_Valid""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every fixed-point type `S', `S'Fixed_Value' denotes a function with\nthe following specification:\n\nfunction S'Fixed_Value (Arg : <Universal_Integer>) return S;\n\nThe value returned is the fixed-point value `V' such that:\n\nV = Arg * S'Small\n\nThe effect is thus similar to first converting the argument to the\ninteger type used to represent `S', and then doing an unchecked\nconversion to the fixed-point type.  The difference is that there are\nfull range checks, to ensure that the result is in range.  This\nattribute is primarily intended for use in implementation of the\ninput-output functions for fixed-point values.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Fixed_Value""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Floor denotes a function with the following specification:\n\n\nfunction S'Floor (X : T)\nreturn T\n\n\nThe function yields the value Floor(X), i.e., the largest\n(most positive) integral value less than or equal to X. When X\nis zero, the result has the sign of X; a zero result otherwise\nhas a positive sign. See A.5.3.""," & ASCII.LF
   & """_id"": ""74""," & ASCII.LF
   & """_name"": ""Floor""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every fixed point subtype S:\n\n\nS'Fore yields the minimum number of characters needed before\nthe decimal point for the decimal representation of any value\nof the subtype S, assuming that the representation does not\ninclude an exponent, but includes a one-character prefix that\nis either a minus sign or a space. (This minimum number does\nnot include superfluous zeros or underlines, and is at least\n) The value of this attribute is of the type\nuniversal_integer. See 3.5.10.""," & ASCII.LF
   & """_id"": ""78""," & ASCII.LF
   & """_name"": ""Fore""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Fraction denotes a function with the following\nspecification:\n\n\nfunction S'Fraction (X : T)\nreturn T\n\n\nThe function yields the value X x T'Machine_Radix(-k), where k\nis the normalized exponent of X. A zero result, which can only\noccur when X is zero, has the sign of X. See A.5.3.""," & ASCII.LF
   & """_id"": ""80""," & ASCII.LF
   & """_name"": ""Fraction""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This internal attribute is used for the generation of remote subprogram\nstubs in the context of the Distributed Systems Annex.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""From_Any""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The prefix of the `Has_Access_Values' attribute is a type.  The result\nis a Boolean value which is True if the is an access type, or is a\ncomposite type with a component (at any nesting depth) that is an\naccess type, and is False otherwise.  The intended use of this\nattribute is in conjunction with generic definitions.  If the attribute\nis applied to a generic private type, it indicates whether or not the\ncorresponding actual type has access values.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Has_Access_Values""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The prefix of the `Has_Discriminants' attribute is a type.  The result\nis a Boolean value which is True if the type has discriminants, and\nFalse otherwise.  The intended use of this attribute is in conjunction\nwith generic definitions.  If the attribute is applied to a generic\nprivate type, it indicates whether or not the corresponding actual type\nhas discriminants.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Has_Discriminants""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes an object:\n\n\nX'Has_Same_Storage denotes a function with the following\nspecification:\n\n\nfunction X'Has_Same_Storage (Arg : any_type)\nreturn Boolean\n\n\nThe actual parameter shall be a name that denotes an object.\nThe object denoted by the actual parameter can be of any type.\nThis function evaluates the names of the objects involved. It\nreturns True if the representation of the object denoted by\nthe actual parameter occupies exactly the same bits as the\nrepresentation of the object denoted by X and the objects\noccupy at least one bit; otherwise, it returns False. See""," & ASCII.LF
   & """_id"": ""83.1/3""," & ASCII.LF
   & """_name"": ""Has_Same_Storage""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix E that denotes an exception:\n\n\nE'Identity returns the unique identity of the exception. The\ntype of this attribute is Exception_Id. See 11.4.1.""," & ASCII.LF
   & """_id"": ""84/1""," & ASCII.LF
   & """_name"": ""Identity""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix T that is of a task type (after any implicit\ndereference):\n\n\nYields a value of the type Task_Id that identifies the task\ndenoted by T. See C.7.1.""," & ASCII.LF
   & """_id"": ""86""," & ASCII.LF
   & """_name"": ""Identity""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Image denotes a function with the following specification:\n\n\nfunction S'Image(Arg : S'Base)\nreturn String\n\n\nThe function returns an image of the value of Arg as a String.\nSee 3.5.""," & ASCII.LF
   & """_id"": ""88""," & ASCII.LF
   & """_name"": ""Image""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes an object of a scalar type (after\nany implicit dereference):\n\n\nX'Image denotes the result of calling function S'Image with\nArg being X, where S is the nominal subtype of X. See 3.5.""," & ASCII.LF
   & """_id"": ""91.1/4""," & ASCII.LF
   & """_name"": ""Image""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Img' attribute differs from `Image' in that, while both can be\napplied directly to an object, `Img' cannot be applied to types.\n\nExample usage of the attribute:\n\nPut_Line (\""X = \"" & X'Img);\n\nwhich has the same meaning as the more verbose:\n\nPut_Line (\""X = \"" & T'Image (X));\n\nwhere `T' is the (sub)type of the object `X'.\n\nNote that technically, in analogy to `Image', `X'Img' returns a\nparameterless function that returns the appropriate string when called.\nThis means that `X'Img' can be renamed as a function-returning-string,\nor used in an instantiation as a function parameter.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Img""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a specific type T:\n\n\nS'Input denotes a function with the following specification:\n\n\nfunction S'Input(\nStream : not null access Ada.Streams.Root_Stream_Type'Class)\nreturn T\n\n\nS'Input reads and returns one value from Stream, using any\nbounds or discriminants written by a corresponding S'Output to\ndetermine how much to read. See 13.13.2.""," & ASCII.LF
   & """_id"": ""96""," & ASCII.LF
   & """_name"": ""Input""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every integer type `S', `S'Integer_Value' denotes a function with\nthe following spec:\n\nfunction S'Integer_Value (Arg : <Universal_Fixed>) return S;\n\nThe value returned is the integer value `V', such that:\n\nArg = V * T'Small\n\nwhere `T' is the type of `Arg'.  The effect is thus similar to first\ndoing an unchecked conversion from the fixed-point type to its\ncorresponding implementation type, and then converting the result to\nthe target integer type.  The difference is that there are full range\nchecks, to ensure that the result is in range.  This attribute is\nprimarily intended for use in implementation of the standard\ninput-output functions for fixed-point values.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Integer_Value""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar type S, S'Invalid_Value returns an undefined value of\nthe type. If possible this value is an invalid representation for the\ntype. The value returned is identical to the value used to initialize\nan otherwise uninitialized value of the type if pragma\nInitialize_Scalars is used, including the ability to modify the value\nwith the binder -Sxx flag and relevant environment variables at run\ntime.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Invalid_Value""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Equivalent to Aspect Iterable.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Iterable""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Large' attribute is provided for compatibility with Ada 83.  See\nthe Ada 83 reference manual for an exact description of the semantics of\nthis attribute.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Large""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix A that is of an array type (after any implicit\ndereference), or denotes a constrained array subtype:\n\n\nA'Last denotes the upper bound of the first index range; its\ntype is the corresponding index type. See 3.6.2.""," & ASCII.LF
   & """_id"": ""100/1""," & ASCII.LF
   & """_name"": ""Last""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Last denotes the upper bound of the range of S. The value of\nthis attribute is of the type of S. See 3.5.""," & ASCII.LF
   & """_id"": ""102""," & ASCII.LF
   & """_name"": ""Last""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix A that is of an array type (after any implicit\ndereference), or denotes a constrained array subtype:\n\n\nA'Last(N) denotes the upper bound of the N-th index range; its\ntype is the corresponding index type. See 3.6.2.""," & ASCII.LF
   & """_id"": ""104/1""," & ASCII.LF
   & """_name"": ""Last(N)""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a component C of a composite, non-array object R:\n\n\nIf the nondefault bit ordering applies to the composite type,\nand if a component_clause specifies the placement of C,\ndenotes the value given for the last_bit of the\ncomponent_clause; otherwise, denotes the offset, from the\nstart of the first of the storage elements occupied by C, of\nthe last bit occupied by C. This offset is measured in bits.\nThe value of this attribute is of the type universal_integer.\nSee 13.5.2.""," & ASCII.LF
   & """_id"": ""106""," & ASCII.LF
   & """_name"": ""Last_Bit""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every static discrete subtype S for which there exists at\nleast one value belonging to S that satisfies the predicates\nof S:\n\n\nS'Last_Valid denotes the largest value that belongs to S and\nsatisfies the predicates of S. The value of this attribute is\nof the type of S. See 3.5.5.""," & ASCII.LF
   & """_id"": ""107.1/4""," & ASCII.LF
   & """_name"": ""Last_Valid""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Leading_Part denotes a function with the following\nspecification:\n\n\nfunction S'Leading_Part (X : T;\nRadix_Digits : universal_integer)\nreturn T\n\n\nLet v be the value T'Machine_Radix(k-Radix_Digits), where k is\nthe normalized exponent of X. The function yields the value\n\n\n* Floor(X/v) x v, when X is nonnegative and Radix_Digits is\npositive;\n\n\n* Ceiling(X/v) x v, when X is negative and Radix_Digits is\npositive.\n\n\nConstraint_Error is raised when Radix_Digits is zero or\nnegative. A zero result, which can only occur when X is zero,\nhas the sign of X. See A.5.3.""," & ASCII.LF
   & """_id"": ""108""," & ASCII.LF
   & """_name"": ""Leading_Part""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix A that is of an array type (after any implicit\ndereference), or denotes a constrained array subtype:\n\n\nA'Length denotes the number of values of the first index range\n(zero for a null range); its type is universal_integer. See""," & ASCII.LF
   & """_id"": ""115/1""," & ASCII.LF
   & """_name"": ""Length""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix A that is of an array type (after any implicit\ndereference), or denotes a constrained array subtype:\n\n\nA'Length(N) denotes the number of values of the N-th index\nrange (zero for a null range); its type is universal_integer.\nSee 3.6.2.""," & ASCII.LF
   & """_id"": ""117/1""," & ASCII.LF
   & """_name"": ""Length(N)""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`P'Library_Level', where P is an entity name, returns a Boolean value\nwhich is True if the entity is declared at the library level, and False\notherwise. Note that within a generic instantition, the name of the\ngeneric unit denotes the instance, which means that this attribute can\nbe used to test if a generic is instantiated at the library level, as\nshown in this example:\n\ngeneric\n\npackage Gen is\npragma Compile_Time_Error\n(not Gen'Library_Level,\n\""Gen can only be instantiated at library level\"");\n\nend Gen;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Library_Level""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`P'Lock_Free', where P is a protected object, returns True if a pragma\n`Lock_Free' applies to P.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Lock_Free""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\nX'Loop_Entry [(loop_name)]\n\nThe `Loop_Entry' attribute is used to refer to the value that an\nexpression had upon entry to a given loop in much the same way that the\n`Old' attribute in a subprogram postcondition can be used to refer to\nthe value an expression had upon entry to the subprogram. The relevant\nloop is either identified by the given loop name, or it is the\ninnermost enclosing loop when no loop name is given.\n\nA `Loop_Entry' attribute can only occur within a `Loop_Variant' or\n`Loop_Invariant' pragma. A common use of `Loop_Entry' is to compare the\ncurrent value of objects with their initial value at loop entry, in a\n`Loop_Invariant' pragma.\n\nThe effect of using `X'Loop_Entry' is the same as declaring a constant\ninitialized with the initial value of `X' at loop entry. This copy is\nnot performed if the loop is not entered, or if the corresponding\npragmas are ignored or disabled.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Loop_Entry""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Machine denotes a function with the following specification:\n\n\nfunction S'Machine (X : T)\nreturn T\n\n\nIf X is a machine number of the type T, the function yields X;\notherwise, it yields the value obtained by rounding or\ntruncating X to either one of the adjacent machine numbers of\nthe type T. Constraint_Error is raised if rounding or\ntruncating X to the precision of the machine numbers results\nin a value outside the base range of S. A zero result has the\nsign of X when S'Signed_Zeros is True. See A.5.3.""," & ASCII.LF
   & """_id"": ""119""," & ASCII.LF
   & """_name"": ""Machine""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nYields the largest (most positive) value of exponent such that\nevery value expressible in the canonical form (for the type\nT), having a mantissa of T'Machine_Mantissa digits, is a\nmachine number (see 3.5.7) of the type T. This attribute\nyields a value of the type universal_integer. See A.5.3.""," & ASCII.LF
   & """_id"": ""123""," & ASCII.LF
   & """_name"": ""Machine_Emax""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nYields the smallest (most negative) value of exponent such\nthat every value expressible in the canonical form (for the\ntype T), having a mantissa of T'Machine_Mantissa digits, is a\nmachine number (see 3.5.7) of the type T. This attribute\nyields a value of the type universal_integer. See A.5.3.""," & ASCII.LF
   & """_id"": ""125""," & ASCII.LF
   & """_name"": ""Machine_Emin""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nYields the largest value of p such that every value\nexpressible in the canonical form (for the type T), having a\np-digit mantissa and an exponent between T'Machine_Emin and\nT'Machine_Emax, is a machine number (see 3.5.7) of the type T.\nThis attribute yields a value of the type universal_integer.\nSee A.5.3.""," & ASCII.LF
   & """_id"": ""127""," & ASCII.LF
   & """_name"": ""Machine_Mantissa""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nYields the value True if overflow and divide-by-zero are\ndetected and reported by raising Constraint_Error for every\npredefined operation that yields a result of the type T;\nyields the value False otherwise. The value of this attribute\nis of the predefined type Boolean. See A.5.3.""," & ASCII.LF
   & """_id"": ""129""," & ASCII.LF
   & """_name"": ""Machine_Overflows""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a fixed point type T:\n\n\nYields the value True if overflow and divide-by-zero are\ndetected and reported by raising Constraint_Error for every\npredefined operation that yields a result of the type T;\nyields the value False otherwise. The value of this attribute\nis of the predefined type Boolean. See A.5.4.""," & ASCII.LF
   & """_id"": ""131""," & ASCII.LF
   & """_name"": ""Machine_Overflows""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nYields the radix of the hardware representation of the type T.\nThe value of this attribute is of the type universal_integer.\nSee A.5.3.""," & ASCII.LF
   & """_id"": ""133""," & ASCII.LF
   & """_name"": ""Machine_Radix""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a fixed point type T:\n\n\nYields the radix of the hardware representation of the type T.\nThe value of this attribute is of the type universal_integer.\nSee A.5.4.""," & ASCII.LF
   & """_id"": ""135""," & ASCII.LF
   & """_name"": ""Machine_Radix""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Machine_Rounding denotes a function with the following\nspecification:\n\n\nfunction S'Machine_Rounding (X : T)\nreturn T\n\n\nThe function yields the integral value nearest to X. If X lies\nexactly halfway between two integers, one of those integers is\nreturned, but which of them is returned is unspecified. A zero\nresult has the sign of X when S'Signed_Zeros is True. This\nfunction provides access to the rounding behavior which is\nmost efficient on the target processor. See A.5.3.""," & ASCII.LF
   & """_id"": ""136.1/2""," & ASCII.LF
   & """_name"": ""Machine_Rounding""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nYields the value True if rounding is performed on inexact\nresults of every predefined operation that yields a result of\nthe type T; yields the value False otherwise. The value of\nthis attribute is of the predefined type Boolean. See A.5.3.""," & ASCII.LF
   & """_id"": ""137""," & ASCII.LF
   & """_name"": ""Machine_Rounds""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a fixed point type T:\n\n\nYields the value True if rounding is performed on inexact\nresults of every predefined operation that yields a result of\nthe type T; yields the value False otherwise. The value of\nthis attribute is of the predefined type Boolean. See A.5.4.""," & ASCII.LF
   & """_id"": ""139""," & ASCII.LF
   & """_name"": ""Machine_Rounds""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This attribute is identical to the `Object_Size' attribute.  It is\nprovided for compatibility with the DEC Ada 83 attribute of this name.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Machine_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Mantissa' attribute is provided for compatibility with Ada 83.  See\nthe Ada 83 reference manual for an exact description of the semantics of\nthis attribute.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Mantissa""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Max denotes a function with the following specification:\n\n\nfunction S'Max(Left, Right : S'Base)\nreturn S'Base\n\n\nThe function returns the greater of the values of the two\nparameters. See 3.5.""," & ASCII.LF
   & """_id"": ""141""," & ASCII.LF
   & """_name"": ""Max""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S:\n\n\nDenotes the maximum value for Alignment that could be\nrequested by the implementation via Allocate for an access\ntype whose designated subtype is S. The value of this\nattribute is of type universal_integer. See 13.11.1.""," & ASCII.LF
   & """_id"": ""144.1/3""," & ASCII.LF
   & """_name"": ""Max_Alignment_For_Allocation""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S:\n\n\nDenotes the maximum value for Size_In_Storage_Elements that\ncould be requested by the implementation via Allocate for an\naccess type whose designated subtype is S. The value of this\nattribute is of type universal_integer. See 13.11.1.""," & ASCII.LF
   & """_id"": ""145""," & ASCII.LF
   & """_name"": ""Max_Size_In_Storage_Elements""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`Standard'Maximum_Alignment' (`Standard' is the only permissible\nprefix) provides the maximum useful alignment value for the target.\nThis is a static value that can be used to specify the alignment for an\nobject, guaranteeing that it is properly aligned in all cases.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Maximum_Alignment""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`func'Mechanism_Code' yields an integer code for the mechanism used for\nthe result of function `func', and `subprog'Mechanism_Code (n)' yields\nthe mechanism used for formal parameter number `n' (a static integer\nvalue, with 1 meaning the first parameter) of subprogram `subprog'.\nThe code returned is:\n\n`1'\nby copy (value)\n\n`2'\nby reference""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Mechanism_Code""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Min denotes a function with the following specification:\n\n\nfunction S'Min(Left, Right : S'Base)\nreturn S'Base\n\n\nThe function returns the lesser of the values of the two\nparameters. See 3.5.""," & ASCII.LF
   & """_id"": ""147""," & ASCII.LF
   & """_name"": ""Min""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every modular subtype S:\n\n\nS'Mod denotes a function with the following specification:\n\n\nfunction S'Mod (Arg : universal_integer)\nreturn S'Base\n\n\nThis function returns Arg mod S'Modulus, as a value of the\ntype of S. See 3.5.4.""," & ASCII.LF
   & """_id"": ""150.1/2""," & ASCII.LF
   & """_name"": ""Mod""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Model denotes a function with the following specification:\n\n\nfunction S'Model (X : T)\nreturn T\n\n\nIf the Numerics Annex is not supported, the meaning of this\nattribute is implementation defined; see G.2.2 for the\ndefinition that applies to implementations supporting the\nNumerics Annex. See A.5.3.""," & ASCII.LF
   & """_id"": ""151""," & ASCII.LF
   & """_name"": ""Model""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nIf the Numerics Annex is not supported, this attribute yields\nan implementation defined value that is greater than or equal\nto the value of T'Machine_Emin. See G.2.2 for further\nrequirements that apply to implementations supporting the\nNumerics Annex. The value of this attribute is of the type\nuniversal_integer. See A.5.3.""," & ASCII.LF
   & """_id"": ""155""," & ASCII.LF
   & """_name"": ""Model_Emin""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nYields the value T'Machine_Radix(1 - T'Model_Mantissa). The\nvalue of this attribute is of the type universal_real. See\nA.5.3.""," & ASCII.LF
   & """_id"": ""157""," & ASCII.LF
   & """_name"": ""Model_Epsilon""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nIf the Numerics Annex is not supported, this attribute yields\nan implementation defined value that is greater than or equal\nto Ceiling(d x log(10) / log(T'Machine_Radix)) + 1, where d is\nthe requested decimal precision of T, and less than or equal\nto the value of T'Machine_Mantissa. See G.2.2 for further\nrequirements that apply to implementations supporting the\nNumerics Annex. The value of this attribute is of the type\nuniversal_integer. See A.5.3.""," & ASCII.LF
   & """_id"": ""159""," & ASCII.LF
   & """_name"": ""Model_Mantissa""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nYields the value T'Machine_Radix(T'Model_Emin - 1). The value\nof this attribute is of the type universal_real. See A.5.3.""," & ASCII.LF
   & """_id"": ""161""," & ASCII.LF
   & """_name"": ""Model_Small""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every modular subtype S:\n\n\nS'Modulus yields the modulus of the type of S, as a value of\nthe type universal_integer. See 3.5.4.""," & ASCII.LF
   & """_id"": ""163""," & ASCII.LF
   & """_name"": ""Modulus""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""A reference `T'Null_Parameter' denotes an imaginary object of type or\nsubtype `T' allocated at machine address zero.  The attribute is\nallowed only as the default expression of a formal parameter, or as an\nactual expression of a subprogram call.  In either case, the subprogram\nmust be imported.\n\nThe identity of the object is represented by the address zero in the\nargument list, independent of the passing mechanism (explicit or\ndefault).\n\nThis capability is needed to specify that a zero address should be\npassed for a record or other composite object passed by reference.\nThere is no way of indicating this without the `Null_Parameter'\nattribute.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Null_Parameter""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The size of an object is not necessarily the same as the size of the\ntype of an object.  This is because by default object sizes are\nincreased to be a multiple of the alignment of the object.  For example,\n`Natural'Size' is 31, but by default objects of type `Natural' will\nhave a size of 32 bits.  Similarly, a record containing an integer and\na character:\n\ntype Rec is record\nI : Integer;\nC : Character;\nend record;\n\nwill have a size of 40 (that is `Rec'Size' will be 40).  The alignment\nwill be 4, because of the integer field, and so the default size of\nrecord objects for this type will be 64 (8 bytes).\n\nIf the alignment of the above record is specified to be 1, then the\nobject size will be 40 (5 bytes). This is true by default, and also an\nobject size of 40 can be explicitly specified in this case.\n\nA consequence of this capability is that different object sizes can be\ngiven to subtypes that would otherwise be considered in Ada to be\nstatically matching.  But it makes no sense to consider such subtypes\nas statically matching.  Consequently, GNAT adds a rule to the static\nmatching rules that requires object sizes to match.  Consider this\nexample:\n\nprocedure BadAVConvert is\ntype R is new Integer;\nsubtype R1 is R range 1 .. 10;\nsubtype R2 is R range 1 .. 10;\nfor R1'Object_Size use 8;\nfor R2'Object_Size use 16;\ntype R1P is access all R1;\ntype R2P is access all R2;\nR1PV : R1P := new R1'(4);\nR2PV : R2P;\nbegin\nR2PV := R2P (R1PV);\n|\n>>> target designated subtype not compatible with\ntype \""R1\"" defined at line 3\n\nend;\n\nIn the absence of lines 5 and 6, types `R1' and `R2' statically match\nand hence the conversion on line 12 is legal. But since lines 5 and 6\ncause the object sizes to differ, GNAT considers that types `R1' and\n`R2' are not statically matching, and line 12 generates the diagnostic\nshown above.\n\nSimilar additional checks are performed in other contexts requiring\nstatically matching subtypes.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Object_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""In addition to the usage of `Old' defined in the Ada 2012 RM (usage\nwithin `Post' aspect), GNAT also permits the use of this attribute in\nimplementation defined pragmas `Postcondition', `Contract_Cases' and\n`Test_Case'. Also usages of `Old' which would be illegal according to\nthe Ada 2012 RM definition are allowed under control of implementation\ndefined pragma `Unevaluated_Use_Of_Old'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Old""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes an object of a nonlimited type:\n\n\nEach X'Old in a postcondition expression that is enabled\ndenotes a constant that is implicitly declared at the\nbeginning of the subprogram body, entry body, or accept\nstatement. See 6.1.1.""," & ASCII.LF
   & """_id"": ""164.1/3""," & ASCII.LF
   & """_name"": ""Old""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a specific type T:\n\n\nS'Output denotes a procedure with the following specification:\n\n\nprocedure S'Output(\nStream : not null access Ada.Streams.Root_Stream_Type'Class;\nItem : in T)\n\n\nS'Output writes the value of Item to Stream, including any\nbounds or discriminants. See 13.13.2.""," & ASCII.LF
   & """_id"": ""169""," & ASCII.LF
   & """_name"": ""Output""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""procedure""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes an object:\n\n\nX'Overlaps_Storage denotes a function with the following\nspecification:\n\n\nfunction X'Overlaps_Storage (Arg : any_type)\nreturn Boolean\n\n\nThe actual parameter shall be a name that denotes an object.\nThe object denoted by the actual parameter can be of any type.\nThis function evaluates the names of the objects involved and\nreturns True if the representation of the object denoted by\nthe actual parameter shares at least one bit with the\nrepresentation of the object denoted by X; otherwise, it\nreturns False. See 13.3.""," & ASCII.LF
   & """_id"": ""172.1/3""," & ASCII.LF
   & """_name"": ""Overlaps_Storage""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix D that denotes a library-level declaration,\nexcepting a declaration of or within a declared-pure library\nunit:\n\n\nDenotes a value of the type universal_integer that identifies\nthe partition in which D was elaborated. If D denotes the\ndeclaration of a remote call interface library unit (see\nE.2.3) the given partition is the one where the body of D was\nelaborated. See E.1.""," & ASCII.LF
   & """_id"": ""173/1""," & ASCII.LF
   & """_name"": ""Partition_Id""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`typ'Passed_By_Reference' for any subtype `typ' returns a value of type\n`Boolean' value that is `True' if the type is normally passed by\nreference and `False' if the type is normally passed by copy in calls.\nFor scalar types, the result is always `False' and is static.  For\nnon-scalar types, the result is nonstatic.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Passed_By_Reference""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`X'Pool_Address' for any object `X' returns the address of X within its\nstorage pool. This is the same as `X'Address', except that for an\nunconstrained array whose bounds are allocated just before the first\ncomponent, `X'Pool_Address' returns the address of those bounds,\nwhereas `X'Address' returns the address of the first component.\n\nHere, we are interpreting 'storage pool' broadly to mean `wherever the\nobject is allocated', which could be a user-defined storage pool, the\nglobal heap, on the stack, or in a static memory area.  For an object\ncreated by `new', `Ptr.all'Pool_Address' is what is passed to\n`Allocate' and returned from `Deallocate'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Pool_Address""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every discrete subtype S:\n\n\nS'Pos denotes a function with the following specification:\n\n\nfunction S'Pos(Arg : S'Base)\nreturn universal_integer\n\n\nThis function returns the position number of the value of Arg,\nas a value of type universal_integer. See 3.5.5.""," & ASCII.LF
   & """_id"": ""175""," & ASCII.LF
   & """_name"": ""Pos""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a component C of a composite, non-array object R:\n\n\nIf the nondefault bit ordering applies to the composite type,\nand if a component_clause specifies the placement of C,\ndenotes the value given for the position of the\ncomponent_clause; otherwise, denotes the same value as\nR.C'Address - R'Address. The value of this attribute is of the\ntype universal_integer. See 13.5.2.""," & ASCII.LF
   & """_id"": ""179""," & ASCII.LF
   & """_name"": ""Position""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Pred denotes a function with the following specification:\n\n\nfunction S'Pred(Arg : S'Base)\nreturn S'Base\n\n\nFor an enumeration type, the function returns the value whose\nposition number is one less than that of the value of Arg;\nConstraint_Error is raised if there is no such value of the\ntype. For an integer type, the function returns the result of\nsubtracting one from the value of Arg. For a fixed point type,\nthe function returns the result of subtracting small from the\nvalue of Arg. For a floating point type, the function returns\nthe machine number (as defined in 3.5.7) immediately below the\nvalue of Arg; Constraint_Error is raised if there is no such\nmachine number. See 3.5.""," & ASCII.LF
   & """_id"": ""181""," & ASCII.LF
   & """_name"": ""Pred""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix P that denotes a protected object:\n\n\nDenotes a non-aliased component of the protected object P.\nThis component is of type System.Any_Priority and its value is\nthe priority of P. P'Priority denotes a variable if and only\nif P denotes a variable. A reference to this attribute shall\nappear only within the body of P. See D.5.2.""," & ASCII.LF
   & """_id"": ""184.1/2""," & ASCII.LF
   & """_name"": ""Priority""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix A that is of an array type (after any implicit\ndereference), or denotes a constrained array subtype:\n\n\nA'Range is equivalent to the range A'First .. A'Last, except\nthat the prefix A is only evaluated once. See 3.6.2.""," & ASCII.LF
   & """_id"": ""185/1""," & ASCII.LF
   & """_name"": ""Range""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Range is equivalent to the range S'First .. S'Last. See""," & ASCII.LF
   & """_id"": ""187""," & ASCII.LF
   & """_name"": ""Range""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix A that is of an array type (after any implicit\ndereference), or denotes a constrained array subtype:\n\n\nA'Range(N) is equivalent to the range A'First(N) .. A'Last(N),\nexcept that the prefix A is only evaluated once. See 3.6.2.""," & ASCII.LF
   & """_id"": ""189/1""," & ASCII.LF
   & """_name"": ""Range(N)""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`typ'Range_Length' for any discrete type `typ' yields the number of\nvalues represented by the subtype (zero for a null range).  The result\nis static for static subtypes.  `Range_Length' applied to the index\nsubtype of a one dimensional array always gives the same result as\n`Length' applied to the array itself.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Range_Length""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a specific type T:\n\n\nS'Read denotes a procedure with the following specification:\n\n\nprocedure S'Read(\nStream : not null access Ada.Streams.Root_Stream_Type'Class;\nItem : out T)\n\n\nS'Read reads the value of Item from Stream. See 13.13.2.""," & ASCII.LF
   & """_id"": ""195""," & ASCII.LF
   & """_name"": ""Read""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""procedure""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Remainder denotes a function with the following\nspecification:\n\n\nfunction S'Remainder (X, Y : T)\nreturn T\n\n\nFor nonzero Y, let v be the value X - n x Y, where n is the\ninteger nearest to the exact value of X/Y; if |n - X/Y| = 1/2,\nthen n is chosen to be even. If v is a machine number of the\ntype T, the function yields v; otherwise, it yields zero.\nConstraint_Error is raised if Y is zero. A zero result has the\nsign of X when S'Signed_Zeros is True. See A.5.3.""," & ASCII.LF
   & """_id"": ""199""," & ASCII.LF
   & """_name"": ""Remainder""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This attribute allows compile time testing of restrictions that are\ncurrently in effect. It is primarily intended for specializing code in\nthe run-time based on restrictions that are active (e.g.  don't need to\nsave fpt registers if restriction No_Floating_Point is known to be in\neffect), but can be used anywhere.\n\nThere are two forms:\n\nSystem'Restriction_Set (partition_boolean_restriction_NAME)\nSystem'Restriction_Set (No_Dependence => library_unit_NAME);\n\nIn the case of the first form, the only restriction names allowed are\nparameterless restrictions that are checked for consistency at bind\ntime. For a complete list see the subtype\n`System.Rident.Partition_Boolean_Restrictions'.\n\nThe result returned is True if the restriction is known to be in\neffect, and False if the restriction is known not to be in effect. An\nimportant guarantee is that the value of a Restriction_Set attribute is\nknown to be consistent throughout all the code of a partition.\n\nThis is trivially achieved if the entire partition is compiled with a\nconsistent set of restriction pragmas. However, the compilation model\ndoes not require this. It is possible to compile one set of units with\none set of pragmas, and another set of units with another set of\npragmas. It is even possible to compile a spec with one set of pragmas,\nand then WITH the same spec with a different set of pragmas.\nInconsistencies in the actual use of the restriction are checked at\nbind time.\n\nIn order to achieve the guarantee of consistency for the\nRestriction_Set pragma, we consider that a use of the pragma that\nyields False is equivalent to a violation of the restriction.\n\nSo for example if you write\n\nif System'Restriction_Set (No_Floating_Point) then\n\nelse\n\nend if;\n\nAnd the result is False, so that the else branch is executed, you can\nassume that this restriction is not set for any unit in the partition.\nThis is checked by considering this use of the restriction pragma to be\na violation of the restriction No_Floating_Point. This means that no\nother unit can attempt to set this restriction (if some unit does\nattempt to set it, the binder will refuse to bind the partition).\n\nTechnical note: The restriction name and the unit name are intepreted\nentirely syntactically, as in the corresponding Restrictions pragma,\nthey are not analyzed semantically, so they do not have a type.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Restriction_Set""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`function'Result' can only be used with in a Postcondition pragma for a\nfunction. The prefix must be the name of the corresponding function.\nThis is used to refer to the result of the function in the\npostcondition expression.  For a further discussion of the use of this\nattribute and examples of its use, see the description of pragma\nPostcondition.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Result""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix F that denotes a function declaration:\n\n\nWithin a postcondition expression for function F, denotes the\nresult object of the function. The type of this attribute is\nthat of the function result except within a Post'Class\npostcondition expression for a function with a controlling\nresult or with a controlling access result. For a controlling\nresult, the type of the attribute is T'Class, where T is the\nfunction result type. For a controlling access result, the\ntype of the attribute is an anonymous access type whose\ndesignated type is T'Class, where T is the designated type of\nthe function result type. See 6.1.1.""," & ASCII.LF
   & """_id"": ""202.1/3""," & ASCII.LF
   & """_name"": ""Result""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every decimal fixed point subtype S:\n\n\nS'Round denotes a function with the following specification:\n\n\nfunction S'Round(X : universal_real)\nreturn S'Base\n\n\nThe function returns the value obtained by rounding X (away\nfrom 0, if X is midway between two values of the type of S).\nSee 3.5.10.""," & ASCII.LF
   & """_id"": ""203""," & ASCII.LF
   & """_name"": ""Round""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Rounding denotes a function with the following\nspecification:\n\n\nfunction S'Rounding (X : T)\nreturn T\n\n\nThe function yields the integral value nearest to X, rounding\naway from zero if X lies exactly halfway between two integers.\nA zero result has the sign of X when S'Signed_Zeros is True.\nSee A.5.3.""," & ASCII.LF
   & """_id"": ""207""," & ASCII.LF
   & """_name"": ""Rounding""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Safe_Emax' attribute is provided for compatibility with Ada 83.\nSee the Ada 83 reference manual for an exact description of the\nsemantics of this attribute.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Safe_Emax""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nYields the lower bound of the safe range (see 3.5.7) of the\ntype T. If the Numerics Annex is not supported, the value of\nthis attribute is implementation defined; see G.2.2 for the\ndefinition that applies to implementations supporting the\nNumerics Annex. The value of this attribute is of the type\nuniversal_real. See A.5.3.""," & ASCII.LF
   & """_id"": ""211""," & ASCII.LF
   & """_name"": ""Safe_First""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Safe_Large' attribute is provided for compatibility with Ada 83.\nSee the Ada 83 reference manual for an exact description of the\nsemantics of this attribute.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Safe_Large""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nYields the upper bound of the safe range (see 3.5.7) of the\ntype T. If the Numerics Annex is not supported, the value of\nthis attribute is implementation defined; see G.2.2 for the\ndefinition that applies to implementations supporting the\nNumerics Annex. The value of this attribute is of the type\nuniversal_real. See A.5.3.""," & ASCII.LF
   & """_id"": ""213""," & ASCII.LF
   & """_name"": ""Safe_Last""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Safe_Small' attribute is provided for compatibility with Ada 83.\nSee the Ada 83 reference manual for an exact description of the\nsemantics of this attribute.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Safe_Small""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every array or record type `S', the representation attribute\n`Scalar_Storage_Order' denotes the order in which storage elements that\nmake up scalar components are ordered within S. The value given must be\na static expression of type System.Bit_Order. The following is an\nexample of the use of this feature:\n\n--  Component type definitions\n\nsubtype Yr_Type is Natural range 0 .. 127;\nsubtype Mo_Type is Natural range 1 .. 12;\nsubtype Da_Type is Natural range 1 .. 31;\n\n--  Record declaration\n\ntype Date is record\nYears_Since_1980 : Yr_Type;\nMonth            : Mo_Type;\nDay_Of_Month     : Da_Type;\nend record;\n\n--  Record representation clause\n\nfor Date use record\nYears_Since_1980 at 0 range 0  ..  6;\nMonth            at 0 range 7  .. 10;\nDay_Of_Month     at 0 range 11 .. 15;\nend record;\n\n--  Attribute definition clauses\n\nfor Date'Bit_Order use System.High_Order_First;\nfor Date'Scalar_Storage_Order use System.High_Order_First;\n--  If Scalar_Storage_Order is specified, it must be consistent with\n--  Bit_Order, so it's best to always define the latter explicitly if\n--  the former is used.\n\nOther properties are as for the standard representation attribute\n`Bit_Order' defined by Ada RM 13.5.3(4). The default is\n`System.Default_Bit_Order'.\n\nFor a record type `T', if `T'Scalar_Storage_Order' is specified\nexplicitly, it shall be equal to `T'Bit_Order'. Note: this means that\nif a `Scalar_Storage_Order' attribute definition clause is not\nconfirming, then the type's `Bit_Order' shall be specified explicitly\nand set to the same value.\n\nDerived types inherit an explicitly set scalar storage order from their\nparent types. This may be overridden for the derived type by giving an\nexplicit scalar storage order for it. However, for a record extension,\nthe derived type must have the same scalar storage order as the parent\ntype.\n\nA component of a record type that is itself a record or an array and\nthat does not start and end on a byte boundary must have have the same\nscalar storage order as the record type. A component of a bit-packed\narray type that is itself a record or an array must have the same\nscalar storage order as the array type.\n\nNo component of a type that has an explicit `Scalar_Storage_Order'\nattribute definition may be aliased.\n\nA confirming `Scalar_Storage_Order' attribute definition clause (i.e.\nwith a value equal to `System.Default_Bit_Order') has no effect.\n\nIf the opposite storage order is specified, then whenever the value of\na scalar component of an object of type `S' is read, the storage\nelements of the enclosing machine scalar are first reversed (before\nretrieving the component value, possibly applying some shift and mask\noperatings on the enclosing machine scalar), and the opposite operation\nis done for writes.\n\nIn that case, the restrictions set forth in 13.5.1(10.3/2) for scalar\ncomponents are relaxed. Instead, the following rules apply:\n\n* the underlying storage elements are those at positions `(position\n+ first_bit / storage_element_size) .. (position + (last_bit +\nstorage_element_size - 1) / storage_element_size)'\n\n* the sequence of underlying storage elements shall have a size no\ngreater than the largest machine scalar\n\n* the enclosing machine scalar is defined as the smallest machine\nscalar starting at a position no greater than `position +\nfirst_bit / storage_element_size' and covering storage elements at\nleast up to `position + (last_bit + storage_element_size - 1) /\nstorage_element_size`'\n\n* the position of the component is interpreted relative to that\nmachine scalar.\n\nIf no scalar storage order is specified for a type (either directly, or\nby inheritance in the case of a derived type), then the default is\nnormally the native ordering of the target, but this default can be\noverridden using pragma `Default_Scalar_Storage_Order'.\n\nIf a component of `T' is itself of a record or array type, the specfied\n`Scalar_Storage_Order' does `not' apply to that nested type: an explicit\nattribute definition clause must be provided for the component type as\nwell if desired.\n\nNote that the scalar storage order only affects the in-memory data\nrepresentation. It has no effect on the representation used by stream\nattributes.\n\nNote that debuggers may be unable to display the correct value of scalar\ncomponents of a type for which the opposite storage order is specified.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Scalar_Storage_Order""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every decimal fixed point subtype S:\n\n\nS'Scale denotes the scale of the subtype S, defined as the\nvalue N such that S'Delta = 10.0**(-N). The scale indicates\nthe position of the point relative to the rightmost\nsignificant digits of values of subtype S. The value of this\nattribute is of the type universal_integer. See 3.5.10.""," & ASCII.LF
   & """_id"": ""215""," & ASCII.LF
   & """_name"": ""Scale""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Scaling denotes a function with the following specification:\n\n\nfunction S'Scaling (X : T;\nAdjustment : universal_integer)\nreturn T\n\n\nLet v be the value X x T'Machine_Radix(Adjustment). If v is a\nmachine number of the type T, or if |v| >= T'Model_Small, the\nfunction yields v; otherwise, it yields either one of the\nmachine numbers of the type T adjacent to v. Constraint_Error\nis optionally raised if v is outside the base range of S. A\nzero result has the sign of X when S'Signed_Zeros is True. See\nA.5.3.""," & ASCII.LF
   & """_id"": ""217""," & ASCII.LF
   & """_name"": ""Scaling""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nYields the value True if the hardware representation for the\ntype T has the capability of representing both positively and\nnegatively signed zeros, these being generated and used by the\npredefined operations of the type T as specified in IEC\n:1989; yields the value False otherwise. The value of this\nattribute is of the predefined type Boolean. See A.5.3.""," & ASCII.LF
   & """_id"": ""221""," & ASCII.LF
   & """_name"": ""Signed_Zeros""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every nonformal, nonderived access-to-object type `Acc', the\nrepresentation attribute `Simple_Storage_Pool' may be specified via an\nattribute_definition_clause (or by specifying the equivalent aspect):\n\nMy_Pool : My_Simple_Storage_Pool_Type;\n\ntype Acc is access My_Data_Type;\n\nfor Acc'Simple_Storage_Pool use My_Pool;\n\nThe name given in an attribute_definition_clause for the\n`Simple_Storage_Pool' attribute shall denote a variable of a 'simple\nstorage pool type' (see pragma `Simple_Storage_Pool_Type').\n\nThe use of this attribute is only allowed for a prefix denoting a type\nfor which it has been specified. The type of the attribute is the type\nof the variable specified as the simple storage pool of the access type,\nand the attribute denotes that variable.\n\nIt is illegal to specify both `Storage_Pool' and `Simple_Storage_Pool'\nfor the same access type.\n\nIf the `Simple_Storage_Pool' attribute has been specified for an access\ntype, then applying the `Storage_Pool' attribute to the type is flagged\nwith a warning and its evaluation raises the exception `Program_Error'.\n\nIf the Simple_Storage_Pool attribute has been specified for an access\ntype `S', then the evaluation of the attribute `S'Storage_Size' returns\nthe result of calling `Storage_Size (S'Simple_Storage_Pool)', which is\nintended to indicate the number of storage elements reserved for the\nsimple storage pool. If the Storage_Size function has not been defined\nfor the simple storage pool type, then this attribute returns zero.\n\nIf an access type `S' has a specified simple storage pool of type\n`SSP', then the evaluation of an allocator for that access type calls\nthe primitive `Allocate' procedure for type `SSP', passing\n`S'Simple_Storage_Pool' as the pool parameter. The detailed semantics\nof such allocators is the same as those defined for allocators in\nsection 13.11 of the `Ada Reference Manual', with the term `simple\nstorage pool' substituted for `storage pool'.\n\nIf an access type `S' has a specified simple storage pool of type\n`SSP', then a call to an instance of the `Ada.Unchecked_Deallocation'\nfor that access type invokes the primitive `Deallocate' procedure for\ntype `SSP', passing `S'Simple_Storage_Pool' as the pool parameter. The\ndetailed semantics of such unchecked deallocations is the same as\ndefined in section 13.11.2 of the Ada Reference Manual, except that the\nterm `simple storage pool' is substituted for `storage pool'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Simple_Storage_Pool""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S:\n\n\nIf S is definite, denotes the size (in bits) that the\nimplementation would choose for the following objects of\nsubtype S:\n\n\n* A record component of subtype S when the record type is\npacked.\n\n\n* The formal parameter of an instance of\nUnchecked_Conversion that converts from subtype S to some\nother subtype.\n\n\nIf S is indefinite, the meaning is implementation defined. The\nvalue of this attribute is of the type universal_integer. See""," & ASCII.LF
   & """_id"": ""223""," & ASCII.LF
   & """_name"": ""Size""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes an object:\n\n\nDenotes the size in bits of the representation of the object.\nThe value of this attribute is of the type universal_integer.\nSee 13.3.""," & ASCII.LF
   & """_id"": ""228/1""," & ASCII.LF
   & """_name"": ""Size""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Small' attribute is defined in Ada 95 (and Ada 2005) only for\nfixed-point types.  GNAT also allows this attribute to be applied to\nfloating-point types for compatibility with Ada 83.  See the Ada 83\nreference manual for an exact description of the semantics of this\nattribute when applied to floating-point types.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Small""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every fixed point subtype S:\n\n\nS'Small denotes the small of the type of S. The value of this\nattribute is of the type universal_real. See 3.5.10.""," & ASCII.LF
   & """_id"": ""230""," & ASCII.LF
   & """_name"": ""Small""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every access-to-object subtype S:\n\n\nDenotes the storage pool of the type of S. The type of this\nattribute is Root_Storage_Pool'Class. See 13.11.""," & ASCII.LF
   & """_id"": ""232""," & ASCII.LF
   & """_name"": ""Storage_Pool""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every access-to-object subtype S:\n\n\nYields the result of calling Storage_Size(S'Storage_Pool),\nwhich is intended to be a measure of the number of storage\nelements reserved for the pool. The type of this attribute is\nuniversal_integer. See 13.11.""," & ASCII.LF
   & """_id"": ""234""," & ASCII.LF
   & """_name"": ""Storage_Size""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix T that denotes a task object (after any implicit\ndereference):\n\n\nDenotes the number of storage elements reserved for the task.\nThe value of this attribute is of the type universal_integer.\nThe Storage_Size includes the size of the task's stack, if\nany. The language does not specify whether or not it includes\nother storage associated with the task (such as the \""task\ncontrol block\"" used by some implementations.) See 13.3.""," & ASCII.LF
   & """_id"": ""236/1""," & ASCII.LF
   & """_name"": ""Storage_Size""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`Standard'Storage_Unit' (`Standard' is the only permissible prefix)\nprovides the same value as `System.Storage_Unit'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Storage_Unit""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of an elementary type T:\n\n\nDenotes the number of bits read from or written to a stream by\nthe default implementations of S'Read and S'Write. Hence, the\nnumber of stream elements required per item of elementary type\nT is:\n\n\nT'Stream_Size / Ada.Streams.Stream_Element'Size\n\n\nThe value of this attribute is of type universal_integer and\nis a multiple of Stream_Element'Size. See 13.13.2.""," & ASCII.LF
   & """_id"": ""237.1/2""," & ASCII.LF
   & """_name"": ""Stream_Size""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The GNAT implementation of remote access-to-classwide types is\norganized as described in AARM section E.4 (20.t): a value of an RACW\ntype (designating a remote object) is represented as a normal access\nvalue, pointing to a \""stub\"" object which in turn contains the necessary\ninformation to contact the designated remote object. A call on any\ndispatching operation of such a stub object does the remote call, if\nnecessary, using the information in the stub object to locate the\ntarget partition, etc.\n\nFor a prefix `T' that denotes a remote access-to-classwide type,\n`T'Stub_Type' denotes the type of the corresponding stub objects.\n\nBy construction, the layout of `T'Stub_Type' is identical to that of\ntype `RACW_Stub_Type' declared in the internal implementation-defined\nunit `System.Partition_Interface'. Use of this attribute will create an\nimplicit dependency on this unit.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Stub_Type""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Succ denotes a function with the following specification:\n\n\nfunction S'Succ(Arg : S'Base)\nreturn S'Base\n\n\nFor an enumeration type, the function returns the value whose\nposition number is one more than that of the value of Arg;\nConstraint_Error is raised if there is no such value of the\ntype. For an integer type, the function returns the result of\nadding one to the value of Arg. For a fixed point type, the\nfunction returns the result of adding small to the value of\nArg. For a floating point type, the function returns the\nmachine number (as defined in 3.5.7) immediately above the\nvalue of Arg; Constraint_Error is raised if there is no such\nmachine number. See 3.5.""," & ASCII.LF
   & """_id"": ""238""," & ASCII.LF
   & """_name"": ""Succ""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`Standard'System_Allocator_Alignment' (`Standard' is the only\npermissible prefix) provides the observable guaranted to be honored by\nthe system allocator (malloc). This is a static value that can be used\nin user storage pools based on malloc either to reject allocation with\nalignment too large or to enable a realignment circuitry if the\nalignment request is larger than this value.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""System_Allocator_Alignment""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a tagged type T (specific or\nclass-wide):\n\n\nS'Tag denotes the tag of the type T (or if T is class-wide,\nthe tag of the root type of the corresponding class). The\nvalue of this attribute is of type Tag. See 3.9.""," & ASCII.LF
   & """_id"": ""242""," & ASCII.LF
   & """_name"": ""Tag""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that is of a class-wide tagged type (after any\nimplicit dereference):\n\n\nX'Tag denotes the tag of X. The value of this attribute is of\ntype Tag. See 3.9.""," & ASCII.LF
   & """_id"": ""244""," & ASCII.LF
   & """_name"": ""Tag""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`Standard'Target_Name' (`Standard' is the only permissible prefix)\nprovides a static string value that identifies the target for the\ncurrent compilation. For GCC implementations, this is the standard gcc\ntarget name without the terminating slash (for example, GNAT 5.0 on\nwindows yields \""i586-pc-mingw32msv\"").""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Target_Name""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix T that is of a task type (after any implicit\ndereference):\n\n\nYields the value True if the task denoted by T is terminated,\nand False otherwise. The value of this attribute is of the\npredefined type Boolean. See 9.9.""," & ASCII.LF
   & """_id"": ""246""," & ASCII.LF
   & """_name"": ""Terminated""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `System'To_Address' (`System' is the only permissible prefix)\ndenotes a function identical to `System.Storage_Elements.To_Address'\nexcept that it is a static attribute.  This means that if its argument\nis a static expression, then the result of the attribute is a static\nexpression.  This means that such an expression can be used in contexts\n(e.g., preelaborable packages) which require a static expression and\nwhere the function call could not be used (since the function call is\nalways nonstatic, even if its argument is static). The argument must be\nin the range -(2**(m-1)) .. 2**m-1, where m is the memory size\n(typically 32 or 64). Negative values are intepreted in a modular\nmanner (e.g., -1 means the same as 16#FFFF_FFFF# on a 32 bits machine).""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""To_Address""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This internal attribute is used for the generation of remote subprogram\nstubs in the context of the Distributed Systems Annex.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""To_Any""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Truncation denotes a function with the following\nspecification:\n\n\nfunction S'Truncation (X : T)\nreturn T\n\n\nThe function yields the value Ceiling(X) when X is negative,\nand Floor(X) otherwise. A zero result has the sign of X when\nS'Signed_Zeros is True. See A.5.3.""," & ASCII.LF
   & """_id"": ""248""," & ASCII.LF
   & """_name"": ""Truncation""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This internal attribute is used for the generation of remote subprogram\nstubs in the context of the Distributed Systems Annex.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""TypeCode""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`typ'Type_Class' for any type or subtype `typ' yields the value of the\ntype class for the full type of `typ'.  If `typ' is a generic formal\ntype, the value is the value for the corresponding actual subtype.  The\nvalue of this attribute is of type `System.Aux_DEC.Type_Class', which\nhas the following definition:\n\ntype Type_Class is\n(Type_Class_Enumeration,\nType_Class_Integer,\nType_Class_Fixed_Point,\nType_Class_Floating_Point,\nType_Class_Array,\nType_Class_Record,\nType_Class_Access,\nType_Class_Task,\nType_Class_Address);\n\nProtected types yield the value `Type_Class_Task', which thus applies\nto all concurrent types.  This attribute is designed to be compatible\nwith the DEC Ada 83 attribute of the same name.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Type_Class""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Type_Key' attribute is applicable to a type or subtype and yields\na value of type Standard.String containing encoded information about\nthe type or subtype. This provides improved compatibility with other\nimplementations that support this attribute.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Type_Key""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a floating point type T:\n\n\nS'Unbiased_Rounding denotes a function with the following\nspecification:\n\n\nfunction S'Unbiased_Rounding (X : T)\nreturn T\n\n\nThe function yields the integral value nearest to X, rounding\ntoward the even integer if X lies exactly halfway between two\nintegers. A zero result has the sign of X when S'Signed_Zeros\nis True. See A.5.3.""," & ASCII.LF
   & """_id"": ""252""," & ASCII.LF
   & """_name"": ""Unbiased_Rounding""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes an aliased view of an object:\n\n\nAll rules and semantics that apply to X'Access (see 3.10.2)\napply also to X'Unchecked_Access, except that, for the\npurposes of accessibility rules and checks, it is as if X were\ndeclared immediately within a library package. See 13.10.""," & ASCII.LF
   & """_id"": ""256""," & ASCII.LF
   & """_name"": ""Unchecked_Access""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Unconstrained_Array' attribute can be used with a prefix that\ndenotes any type or subtype. It is a static attribute that yields\n`True' if the prefix designates an unconstrained array, and `False'\notherwise. In a generic instance, the result is still static, and\nyields the result of applying this test to the generic actual.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unconstrained_Array""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The prefix of `Universal_Literal_String' must be a named number.  The\nstatic result is the string consisting of the characters of the number\nas defined in the original source.  This allows the user program to\naccess the actual text of named numbers without intermediate\nconversions and without the need to enclose the strings in quotes (which\nwould preclude their use as numbers).\n\nFor example, the following program prints the first 50 digits of pi:\n\nwith Text_IO; use Text_IO;\nwith Ada.Numerics;\nprocedure Pi is\nbegin\nPut (Ada.Numerics.Pi'Universal_Literal_String);\nend;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Universal_Literal_String""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Unrestricted_Access' attribute is similar to `Access' except that\nall accessibility and aliased view checks are omitted.  This is a\nuser-beware attribute.\n\nFor objects, it is similar to `Address', for which it is a desirable\nreplacement where the value desired is an access type.  In other words,\nits effect is similar to first applying the `Address' attribute and\nthen doing an unchecked conversion to a desired access type.\n\nFor subprograms, `P'Unrestricted_Access' may be used where `P'Access'\nwould be illegal, to construct a value of a less-nested named access\ntype that designates a more-nested subprogram. This value may be used\nin indirect calls, so long as the more-nested subprogram still exists;\nonce the subprogram containing it has returned, such calls are\nerroneous. For example:\n\npackage body P is\n\ntype Less_Nested is not null access procedure;\nGlobal : Less_Nested;\n\nprocedure P1 is\nbegin\nGlobal.all;\nend P1;\n\nprocedure P2 is\nLocal_Var : Integer;\n\nprocedure More_Nested is\nbegin\nLocal_Var ...\nend More_Nested;\nbegin\nGlobal := More_Nested'Unrestricted_Access;\nP1;\nend P2;\n\nend P;\n\nWhen P1 is called from P2, the call via Global is OK, but if P1 were\ncalled after P2 returns, it would be an erroneous use of a dangling\npointer.\n\nFor objects, it is possible to use `Unrestricted_Access' for any type.\nHowever, if the result is of an access-to-unconstrained array subtype,\nthen the resulting pointer has the same scope as the context of the\nattribute, and must not be returned to some enclosing scope.  For\ninstance, if a function uses `Unrestricted_Access' to create an\naccess-to-unconstrained-array and returns that value to the caller, the\nresult will involve dangling pointers. In addition, it is only valid to\ncreate pointers to unconstrained arrays using this attribute if the\npointer has the normal default 'fat' representation where a pointer has\ntwo components, one points to the array and one points to the bounds.\nIf a size clause is used to force 'thin' representation for a pointer\nto unconstrained where there is only space for a single pointer, then\nthe resulting pointer is not usable.\n\nIn the simple case where a direct use of Unrestricted_Access attempts\nto make a thin pointer for a non-aliased object, the compiler will\nreject the use as illegal, as shown in the following example:\n\nwith System; use System;\nprocedure SliceUA2 is\ntype A is access all String;\nfor A'Size use Standard'Address_Size;\n\nprocedure P (Arg : A) is\nbegin\nnull;\nend P;\n\nX : String := \""hello world!\"";\nX2 : aliased String := \""hello world!\"";\n\nAV : A := X'Unrestricted_Access;    -- ERROR\n|\n>>> illegal use of Unrestricted_Access attribute\n>>> attempt to generate thin pointer to unaliased object\n\nbegin\nP (X'Unrestricted_Access);          -- ERROR\n|\n>>> illegal use of Unrestricted_Access attribute\n>>> attempt to generate thin pointer to unaliased object\n\nP (X(7 .. 12)'Unrestricted_Access); -- ERROR\n|\n>>> illegal use of Unrestricted_Access attribute\n>>> attempt to generate thin pointer to unaliased object\n\nP (X2'Unrestricted_Access);         -- OK\nend;\n\nbut other cases cannot be detected by the compiler, and are considered\nto be erroneous. Consider the following example:\n\nwith System; use System;\nwith System; use System;\nprocedure SliceUA is\ntype AF is access all String;\n\ntype A is access all String;\nfor A'Size use Standard'Address_Size;\n\nprocedure P (Arg : A) is\nbegin\nif Arg'Length /= 6 then\nraise Program_Error;\nend if;\nend P;\n\nX : String := \""hello world!\"";\nY : AF := X (7 .. 12)'Unrestricted_Access;\n\nbegin\nP (A (Y));\nend;\n\nA normal unconstrained array value or a constrained array object marked\nas aliased has the bounds in memory just before the array, so a thin\npointer can retrieve both the data and the bounds.  But in this case,\nthe non-aliased object `X' does not have the bounds before the string.\nIf the size clause for type `A' were not present, then the pointer\nwould be a fat pointer, where one component is a pointer to the bounds,\nand all would be well.  But with the size clause present, the\nconversion from fat pointer to thin pointer in the call loses the\nbounds, and so this is erroneous, and the program likely raises a\n`Program_Error' exception.\n\nIn general, it is advisable to completely avoid mixing the use of thin\npointers and the use of `Unrestricted_Access' where the designated type\nis an unconstrained array.  The use of thin pointers should be\nrestricted to cases of porting legacy code that implicitly assumes the\nsize of pointers, and such code should not in any case be using this\nattribute.\n\nAnother erroneous situation arises if the attribute is applied to a\nconstant. The resulting pointer can be used to access the constant, but\nthe effect of trying to modify a constant in this manner is not\nwell-defined. Consider this example:\n\nP : constant Integer := 4;\ntype R is access all Integer;\nRV : R := P'Unrestricted_Access;\n\nRV.all := 3;\n\nHere we attempt to modify the constant P from 4 to 3, but the compiler\nmay or may not notice this attempt, and subsequent references to P may\nyield either the value 3 or the value 4 or the assignment may blow up\nif the compiler decides to put P in read-only memory. One particular\ncase where `Unrestricted_Access' can be used in this way is to modify\nthe value of an `in' parameter:\n\nprocedure K (S : in String) is\ntype R is access all Character;\nRV : R := S (3)'Unrestricted_Access;\nbegin\nRV.all := 'a';\nend;\n\nIn general this is a risky approach. It may appear to \""work\"" but such\nuses of `Unrestricted_Access' are potentially non-portable, even from\none version of GNAT to another, so are best avoided if possible.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unrestricted_Access""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `Update' attribute creates a copy of an array or record value with\none or more modified components. The syntax is:\n\nPREFIX'Update ( RECORD_COMPONENT_ASSOCIATION_LIST )\nPREFIX'Update ( ARRAY_COMPONENT_ASSOCIATION {, ARRAY_COMPONENT_ASSOCIATION } )\nPREFIX'Update ( MULTIDIMENSIONAL_ARRAY_COMPONENT_ASSOCIATION\n{, MULTIDIMENSIONAL_ARRAY_COMPONENT_ASSOCIATION } )\n\nMULTIDIMENSIONAL_ARRAY_COMPONENT_ASSOCIATION ::= INDEX_EXPRESSION_LIST_LIST => EXPRESSION\nINDEX_EXPRESSION_LIST_LIST                   ::= INDEX_EXPRESSION_LIST {| INDEX_EXPRESSION_LIST }\nINDEX_EXPRESSION_LIST                        ::= ( EXPRESSION {, EXPRESSION } )\n\nwhere `PREFIX' is the name of an array or record object, the\nassociation list in parentheses does not contain an `others' choice and\nthe box symbol `<>' may not appear in any expression. The effect is to\nyield a copy of the array or record value which is unchanged apart from\nthe components mentioned in the association list, which are changed to\nthe indicated value. The original value of the array or record value is\nnot affected. For example:\n\ntype Arr is Array (1 .. 5) of Integer;\n\nAvar1 : Arr := (1,2,3,4,5);\nAvar2 : Arr := Avar1'Update (2 => 10, 3 .. 4 => 20);\n\nyields a value for `Avar2' of 1,10,20,20,5 with `Avar1' begin\nunmodified. Similarly:\n\ntype Rec is A, B, C : Integer;\n\nRvar1 : Rec := (A => 1, B => 2, C => 3);\nRvar2 : Rec := Rvar1'Update (B => 20);\n\nyields a value for `Rvar2' of (A => 1, B => 20, C => 3), with `Rvar1'\nbeing unmodifed.  Note that the value of the attribute reference is\ncomputed completely before it is used. This means that if you write:\n\nAvar1 := Avar1'Update (1 => 10, 2 => Function_Call);\n\nthen the value of `Avar1' is not modified if `Function_Call' raises an\nexception, unlike the effect of a series of direct assignments to\nelements of `Avar1'. In general this requires that two extra complete\ncopies of the object are required, which should be kept in mind when\nconsidering efficiency.\n\nThe `Update' attribute cannot be applied to prefixes of a limited type,\nand cannot reference discriminants in the case of a record type.  The\naccessibility level of an Update attribute result object is defined as\nfor an aggregate.\n\nIn the record case, no component can be mentioned more than once. In\nthe array case, two overlapping ranges can appear in the association\nlist, in which case the modifications are processed left to right.\n\nMulti-dimensional arrays can be modified, as shown by this example:\n\nA : array (1 .. 10, 1 .. 10) of Integer;\n\nA := A'Update ((1, 2) => 20, (3, 4) => 30);\n\nwhich changes element (1,2) to 20 and (3,4) to 30.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Update""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `'VADS_Size' attribute is intended to make it easier to port legacy\ncode which relies on the semantics of `'Size' as implemented by the\nVADS Ada 83 compiler.  GNAT makes a best effort at duplicating the same\nsemantic interpretation.  In particular, `'VADS_Size' applied to a\npredefined or other primitive type with no Size clause yields the\nObject_Size (for example, `Natural'Size' is 32 rather than 31 on\ntypical machines).  In addition `'VADS_Size' applied to an object gives\nthe result that would be obtained by applying the attribute to the\ncorresponding type.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""VADS_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every discrete subtype S:\n\n\nS'Val denotes a function with the following specification:\n\n\nfunction S'Val(Arg : universal_integer)\nreturn S'Base\n\n\nThis function returns a value of the type of S whose position\nnumber equals the value of Arg. See 3.5.5.""," & ASCII.LF
   & """_id"": ""258""," & ASCII.LF
   & """_name"": ""Val""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes a scalar object (after any\nimplicit dereference):\n\n\nYields True if and only if the object denoted by X is normal,\nhas a valid representation, and then, if the preceding\nconditions hold, the value of X also satisfies the predicates\nof the nominal subtype of X. The value of this attribute is of\nthe predefined type Boolean. See 13.9.2.""," & ASCII.LF
   & """_id"": ""262""," & ASCII.LF
   & """_name"": ""Valid""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""The `'Valid_Scalars' attribute is intended to make it easier to check\nthe validity of scalar subcomponents of composite objects. The\nattribute is defined for any prefix `P' which denotes an object. Prefix\n`P' can be any type except for tagged private or `Unchecked_Union'\ntypes. The value of the attribute is of type `Boolean'.\n\n`P'Valid_Scalars' yields `True' if and only if the evaluation of\n`C'Valid' yields `True' for every scalar subcomponent `C' of `P', or if\n`P' has no scalar subcomponents. Attribute `'Valid_Scalars' is\nequivalent to attribute `'Valid' for scalar types.\n\nIt is not specified in what order the subcomponents are checked, nor\nwhether any more are checked after any one of them is determined to be\ninvalid. If the prefix `P' is of a class-wide type `T'Class' (where `T'\nis the associated specific type), or if the prefix `P' is of a specific\ntagged type `T', then only the subcomponents of `T' are checked; in\nother words, components of extensions of `T' are not checked even if\n`T'Class (P)'Tag /= T'Tag'.\n\nThe compiler will issue a warning if it can be determined at compile\ntime that the prefix of the attribute has no scalar subcomponents.\n\nNote: `Valid_Scalars' can generate a lot of code, especially in the\ncase of a large variant record. If the attribute is called in many\nplaces in the same program applied to objects of the same type, it can\nreduce program size to write a function with a single use of the\nattribute, and then call that function from multiple places.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Valid_Scalars""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""unknown""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Value denotes a function with the following specification:\n\n\nfunction S'Value(Arg : String)\nreturn S'Base\n\n\nThis function returns a value given an image of the value as a\nString, ignoring any leading or trailing spaces. See 3.5.""," & ASCII.LF
   & """_id"": ""264""," & ASCII.LF
   & """_name"": ""Value""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`type'Value_Size' is the number of bits required to represent a value\nof the given subtype.  It is the same as `type'Size', but, unlike\n`Size', may be set for non-first subtypes.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Value_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix P that statically denotes a program unit:\n\n\nYields a value of the predefined type String that identifies\nthe version of the compilation unit that contains the\ndeclaration of the program unit. See E.3.""," & ASCII.LF
   & """_id"": ""268/1""," & ASCII.LF
   & """_name"": ""Version""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`Standard'Wchar_T_Size' (`Standard' is the only permissible prefix)\nprovides the size in bits of the C `wchar_t' type primarily for\nconstructing the definition of this type in package `Interfaces.C'. The\nresult is a static constant.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Wchar_T_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Wide_Image denotes a function with the following\nspecification:\n\n\nfunction S'Wide_Image(Arg : S'Base)\nreturn Wide_String\n\n\nThe function returns an image of the value of Arg as a\nWide_String. See 3.5.""," & ASCII.LF
   & """_id"": ""270""," & ASCII.LF
   & """_name"": ""Wide_Image""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes an object of a scalar type (after\nany implicit dereference):\n\n\nX'Wide_Image denotes the result of calling function\nS'Wide_Image with Arg being X, where S is the nominal subtype\nof X. See 3.5.""," & ASCII.LF
   & """_id"": ""273.1/4""," & ASCII.LF
   & """_name"": ""Wide_Image""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Wide_Value denotes a function with the following\nspecification:\n\n\nfunction S'Wide_Value(Arg : Wide_String)\nreturn S'Base\n\n\nThis function returns a value given an image of the value as a\nWide_String, ignoring any leading or trailing spaces. See""," & ASCII.LF
   & """_id"": ""274""," & ASCII.LF
   & """_name"": ""Wide_Value""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Wide_Wide_Image denotes a function with the following\nspecification:\n\n\nfunction S'Wide_Wide_Image(Arg : S'Base)\nreturn Wide_Wide_String\n\n\nThe function returns an image of the value of Arg, that is, a\nsequence of characters representing the value in display form.\nSee 3.5.""," & ASCII.LF
   & """_id"": ""277.1/2""," & ASCII.LF
   & """_name"": ""Wide_Wide_Image""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For a prefix X that denotes an object of a scalar type (after\nany implicit dereference):\n\n\nX'Wide_Wide_Image denotes the result of calling function\nS'Wide_Wide_Image with Arg being X, where S is the nominal\nsubtype of X. See 3.5.""," & ASCII.LF
   & """_id"": ""277.5/4""," & ASCII.LF
   & """_name"": ""Wide_Wide_Image""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Wide_Wide_Value denotes a function with the following\nspecification:\n\n\nfunction S'Wide_Wide_Value(Arg : Wide_Wide_String)\nreturn S'Base\n\n\nThis function returns a value given an image of the value as a\nWide_Wide_String, ignoring any leading or trailing spaces. See""," & ASCII.LF
   & """_id"": ""277.7/2""," & ASCII.LF
   & """_name"": ""Wide_Wide_Value""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Wide_Wide_Width denotes the maximum length of a\nWide_Wide_String returned by S'Wide_Wide_Image over all values\nof the subtype S. It denotes zero for a subtype that has a\nnull range. Its type is universal_integer. See 3.5.""," & ASCII.LF
   & """_id"": ""277.11/2""," & ASCII.LF
   & """_name"": ""Wide_Wide_Width""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Wide_Width denotes the maximum length of a Wide_String\nreturned by S'Wide_Image over all values of the subtype S. It\ndenotes zero for a subtype that has a null range. Its type is\nuniversal_integer. See 3.5.""," & ASCII.LF
   & """_id"": ""278""," & ASCII.LF
   & """_name"": ""Wide_Width""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every scalar subtype S:\n\n\nS'Width denotes the maximum length of a String returned by\nS'Image over all values of the subtype S. It denotes zero for\na subtype that has a null range. Its type is\nuniversal_integer. See 3.5.""," & ASCII.LF
   & """_id"": ""280""," & ASCII.LF
   & """_name"": ""Width""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""`Standard'Word_Size' (`Standard' is the only permissible prefix)\nprovides the value `System.Word_Size'. The result is a static constant.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Word_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""For every subtype S of a specific type T:\n\n\nS'Write denotes a procedure with the following specification:\n\n\nprocedure S'Write(\nStream : not null access Ada.Streams.Root_Stream_Type'Class;\nItem : in T)\n\n\nS'Write writes the value of Item to Stream. See 13.13.2.""," & ASCII.LF
   & """_id"": ""286""," & ASCII.LF
   & """_name"": ""Write""," & ASCII.LF
   & """_origin"": ""Ada RM""," & ASCII.LF
   & """_category"": ""procedure""" & ASCII.LF
   & "}" & ASCII.LF
   & "]," & ASCII.LF
   & """PRAGMA"": [" & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Abort_Defer;\n\nThis pragma must appear at the start of the statement sequence of a\nhandled sequence of statements (right after the `begin').  It has the\neffect of deferring aborts for the sequence of statements (but not for\nthe declarations or handlers, if any, associated with this statement\nsequence).""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Abort_Defer""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Abstract_State (ABSTRACT_STATE_LIST);\n\nABSTRACT_STATE_LIST ::=\nnull\n|  STATE_NAME_WITH_OPTIONS\n| (STATE_NAME_WITH_OPTIONS {, STATE_NAME_WITH_OPTIONS} )\n\nSTATE_NAME_WITH_OPTIONS ::=\nSTATE_NAME\n| (STATE_NAME with OPTION_LIST)\n\nOPTION_LIST ::= OPTION {, OPTION}\n\nOPTION ::=\nSIMPLE_OPTION\n| NAME_VALUE_OPTION\n\nSIMPLE_OPTION ::= Ghost | Synchronous\n\nNAME_VALUE_OPTION ::=\nPart_Of => ABSTRACT_STATE\n| External [=> EXTERNAL_PROPERTY_LIST]\n\nEXTERNAL_PROPERTY_LIST ::=\nEXTERNAL_PROPERTY\n| (EXTERNAL_PROPERTY {, EXTERNAL_PROPERTY} )\n\nEXTERNAL_PROPERTY ::=\nAsync_Readers    [=> boolean_EXPRESSION]\n| Async_Writers    [=> boolean_EXPRESSION]\n| Effective_Reads  [=> boolean_EXPRESSION]\n| Effective_Writes [=> boolean_EXPRESSION]\nothers            => boolean_EXPRESSION\n\nSTATE_NAME ::= defining_identifier\n\nABSTRACT_STATE ::= name\n\nFor the semantics of this pragma, see the entry for aspect\n`Abstract_State' in the SPARK 2014 Reference Manual, section 7.1.4.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Abstract_State""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Acc_Data ([ ACC_DATA_CLAUSE [, ACC_DATA_CLAUSE...]]);\n\nACC_DATA_CLAUSE ::=\nCopy          => IDENTIFIERS\n| Copy_In       => IDENTIFIERS\n| Copy_Out      => IDENTIFIERS\n| Create        => IDENTIFIERS\n| Device_Ptr    => IDENTIFIERS\n| Present       => IDENTIFIERS\n\nRequires the `-fopenacc' flag.\n\nEquivalent to the `data' directive of the OpenAcc standard. This pragma\nshould be placed in loops.\n\nFor more information about the effect of the clauses, see the OpenAcc\nspecification.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Acc_Data""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Acc_Kernels [( ACC_KERNELS_CLAUSE [, ACC_KERNELS_CLAUSE...])];\n\nACC_KERNELS_CLAUSE ::=\nAcc_If        => boolean_EXPRESSION\n| Async         => integer_EXPRESSION\n| Copy          => IDENTIFIERS\n| Copy_In       => IDENTIFIERS\n| Copy_Out      => IDENTIFIERS\n| Create        => IDENTIFIERS\n| Default       => None\n| Device_Ptr    => IDENTIFIERS\n| Num_Gangs     => integer_EXPRESSION\n| Num_Workers   => integer_EXPRESSION\n| Present       => IDENTIFIERS\n| Vector_Length => integer_EXPRESSION\n| Wait          => INTEGERS\n\nIDENTIFIERS ::=\n| IDENTIFIER\n| (IDENTIFIER, IDENTIFIERS)\n\nINTEGERS ::=\n| integer_EXPRESSION\n| (integer_EXPRESSION, INTEGERS)\n\nRequires the `-fopenacc' flag.\n\nEquivalent to the kernels directive of the OpenAcc standard. This\npragma should be placed in loops.\n\nFor more information about the effect of the clauses, see the OpenAcc\nspecification.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Acc_Kernels""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Acc_Loop [( ACC_LOOP_CLAUSE [, ACC_LOOP_CLAUSE... ])];\n\nACC_LOOP_CLAUSE ::=\nAuto\n| Collapse        => INTEGER_LITERAL\n| Gang            [=> GANG_ARG]\n| Independent\n| Private         => IDENTIFIERS\n| Reduction       => (REDUCTION_RECORD)\n| Seq\n| Tile            => SIZE_EXPRESSION\n| Vector          [=> integer_EXPRESSION]\n| Worker          [=> integer_EXPRESSION]\n\nGANG_ARG ::=\ninteger_EXPRESSION\n| Static => SIZE_EXPRESSION\n\nSIZE_EXPRESSION ::=\n*\n| integer_EXPRESSION\n\nRequires the `-fopenacc' flag.\n\nEquivalent to the `loop' directive of the OpenAcc standard. This pragma\nshould be placed in for loops after the \""Acc_Parallel\"" pragma. It tells\nthe compiler how to parallelize the loop.\n\nFor more information about the effect of the clauses, see the OpenAcc\nspecification.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Acc_Loop""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Acc_Parallel [( ACC_PARALLEL_CLAUSE [, ACC_PARALLEL_CLAUSE... ])];\n\nACC_PARALLEL_CLAUSE ::=\nAcc_If        => boolean_EXPRESSION\n| Acc_Private   => IDENTIFIERS\n| Async         => integer_EXPRESSION\n| Copy          => IDENTIFIERS\n| Copy_In       => IDENTIFIERS\n| Copy_Out      => IDENTIFIERS\n| Create        => IDENTIFIERS\n| Default       => None\n| Device_Ptr    => IDENTIFIERS\n| First_Private => IDENTIFIERS\n| Num_Gangs     => integer_EXPRESSION\n| Num_Workers   => integer_EXPRESSION\n| Present       => IDENTIFIERS\n| Reduction     => (REDUCTION_RECORD)\n| Vector_Length => integer_EXPRESSION\n| Wait          => INTEGERS\n\nREDUCTION_RECORD ::=\n\""+\""   => IDENTIFIERS\n| \""*\""   => IDENTIFIERS\n| \""min\"" => IDENTIFIERS\n| \""max\"" => IDENTIFIERS\n| \""or\""  => IDENTIFIERS\n| \""and\"" => IDENTIFIERS\n\nIDENTIFIERS ::=\n| IDENTIFIER\n| (IDENTIFIER, IDENTIFIERS)\n\nINTEGERS ::=\n| integer_EXPRESSION\n| (integer_EXPRESSION, INTEGERS)\n\nRequires the `-fopenacc' flag.\n\nEquivalent to the `parallel' directive of the OpenAcc standard. This\npragma should be placed in loops. It offloads the content of the loop\nto an accelerator device.\n\nFor more information about the effect of the clauses, see the OpenAcc\nspecification.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Acc_Parallel""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Ada_05;\npragma Ada_05 (local_NAME);\n\nA configuration pragma that establishes Ada 2005 mode for the unit to\nwhich it applies, regardless of the mode set by the command line\nswitches.  This pragma is useful when writing a reusable component that\nitself uses Ada 2005 features, but which is intended to be usable from\neither Ada 83 or Ada 95 programs.\n\nThe one argument form (which is not a configuration pragma) is used for\nmanaging the transition from Ada 95 to Ada 2005 in the run-time\nlibrary. If an entity is marked as Ada_2005 only, then referencing the\nentity in Ada_83 or Ada_95 mode will generate a warning. In addition,\nin Ada_83 or Ada_95 mode, a preference rule is established which does\nnot choose such an entity unless it is unambiguously specified. This\navoids extra subprograms marked this way from generating ambiguities in\notherwise legal pre-Ada_2005 programs. The one argument form is\nintended for exclusive use in the GNAT run-time library.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Ada_05""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Ada_12;\npragma Ada_12 (local_NAME);\n\nA configuration pragma that establishes Ada 2012 mode for the unit to\nwhich it applies, regardless of the mode set by the command line\nswitches.  This mode is set automatically for the `Ada' and `System'\npackages and their children, so you need not specify it in these\ncontexts.  This pragma is useful when writing a reusable component that\nitself uses Ada 2012 features, but which is intended to be usable from\nAda 83, Ada 95, or Ada 2005 programs.\n\nThe one argument form, which is not a configuration pragma, is used for\nmanaging the transition from Ada 2005 to Ada 2012 in the run-time\nlibrary. If an entity is marked as Ada_2012 only, then referencing the\nentity in any pre-Ada_2012 mode will generate a warning. In addition,\nin any pre-Ada_2012 mode, a preference rule is established which does\nnot choose such an entity unless it is unambiguously specified. This\navoids extra subprograms marked this way from generating ambiguities in\notherwise legal pre-Ada_2012 programs. The one argument form is\nintended for exclusive use in the GNAT run-time library.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Ada_12""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Ada_2005;\n\nThis configuration pragma is a synonym for pragma Ada_05 and has the\nsame syntax and effect.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Ada_2005""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Ada_2012;\n\nThis configuration pragma is a synonym for pragma Ada_12 and has the\nsame syntax and effect.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Ada_2012""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Ada_83;\n\nA configuration pragma that establishes Ada 83 mode for the unit to\nwhich it applies, regardless of the mode set by the command line\nswitches.  In Ada 83 mode, GNAT attempts to be as compatible with the\nsyntax and semantics of Ada 83, as defined in the original Ada 83\nReference Manual as possible.  In particular, the keywords added by Ada\n95 and Ada 2005 are not recognized, optional package bodies are allowed,\nand generics may name types with unknown discriminants without using\nthe `(<>)' notation.  In addition, some but not all of the additional\nrestrictions of Ada 83 are enforced.\n\nAda 83 mode is intended for two purposes.  Firstly, it allows existing\nAda 83 code to be compiled and adapted to GNAT with less effort.\nSecondly, it aids in keeping code backwards compatible with Ada 83.\nHowever, there is no guarantee that code that is processed correctly by\nGNAT in Ada 83 mode will in fact compile and execute with an Ada 83\ncompiler, since GNAT does not enforce all the additional checks\nrequired by Ada 83.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Ada_83""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Ada_95;\n\nA configuration pragma that establishes Ada 95 mode for the unit to\nwhich it applies, regardless of the mode set by the command line\nswitches.  This mode is set automatically for the `Ada' and `System'\npackages and their children, so you need not specify it in these\ncontexts.  This pragma is useful when writing a reusable component that\nitself uses Ada 95 features, but which is intended to be usable from\neither Ada 83 or Ada 95 programs.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Ada_95""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Aggregate_Individually_Assign;\n\nWhere possible, GNAT will store the binary representation of a record\naggregate in memory for space and performance reasons. This\nconfiguration pragma changes this behavior so that record aggregates\nare instead always converted into individual assignment statements.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Aggregate_Individually_Assign""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma All_Calls_Remote [(library_unit_name)];""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""All_Calls_Remote""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Allow_Integer_Address;\n\nIn almost all versions of GNAT, `System.Address' is a private type in\naccordance with the implementation advice in the RM. This means that\ninteger values, in particular integer literals, are not allowed as\naddress values.  If the configuration pragma `Allow_Integer_Address' is\ngiven, then integer expressions may be used anywhere a value of type\n`System.Address' is required.  The effect is to introduce an implicit\nunchecked conversion from the integer value to type `System.Address'.\nThe reverse case of using an address where an integer type is required\nis handled analogously.  The following example compiles without errors:\n\npragma Allow_Integer_Address;\nwith System; use System;\npackage AddrAsInt is\nX : Integer;\nY : Integer;\nfor X'Address use 16#1240#;\nfor Y use at 16#3230#;\nm : Address := 16#4000#;\nn : constant Address := 4000;\np : constant Address := Address (X + Y);\nv : Integer := y'Address;\nw : constant Integer := Integer (Y'Address);\ntype R is new integer;\nRR : R := 1000;\nZ : Integer;\nfor Z'Address use RR;\nend AddrAsInt;\n\nNote that pragma `Allow_Integer_Address' is ignored if `System.Address'\nis not a private type. In implementations of `GNAT' where\nSystem.Address is a visible integer type, this pragma serves no purpose\nbut is ignored rather than rejected to allow common sets of sources to\nbe used in the two situations.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Allow_Integer_Address""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Annotate (IDENTIFIER [, IDENTIFIER {, ARG}] [, entity => local_NAME]);\n\nARG ::= NAME | EXPRESSION\n\nThis pragma is used to annotate programs.  IDENTIFIER identifies the\ntype of annotation.  GNAT verifies that it is an identifier, but does\nnot otherwise analyze it. The second optional identifier is also left\nunanalyzed, and by convention is used to control the action of the tool\nto which the annotation is addressed.  The remaining ARG arguments can\nbe either string literals or more generally expressions.  String\nliterals are assumed to be either of type `Standard.String' or else\n`Wide_String' or `Wide_Wide_String' depending on the character literals\nthey contain.  All other kinds of arguments are analyzed as\nexpressions, and must be unambiguous. The last argument if present must\nhave the identifier `Entity' and GNAT verifies that a local name is\ngiven.\n\nThe analyzed pragma is retained in the tree, but not otherwise processed\nby any part of the GNAT compiler, except to generate corresponding note\nlines in the generated ALI file. For the format of these note lines, see\nthe compiler source file lib-writ.ads. This pragma is intended for use\nby external tools, including ASIS. The use of pragma Annotate does not\naffect the compilation process in any way. This pragma may be used as a\nconfiguration pragma.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Annotate""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Assert (\nboolean_EXPRESSION\n[, string_EXPRESSION]);\n\nThe effect of this pragma depends on whether the corresponding command\nline switch is set to activate assertions.  The pragma expands into code\nequivalent to the following:\n\nif assertions-enabled then\nif not boolean_EXPRESSION then\nSystem.Assertions.Raise_Assert_Failure\n(string_EXPRESSION);\nend if;\nend if;\n\nThe string argument, if given, is the message that will be associated\nwith the exception occurrence if the exception is raised.  If no second\nargument is given, the default message is `file':`nnn', where `file' is\nthe name of the source file containing the assert, and `nnn' is the\nline number of the assert.\n\nNote that, as with the `if' statement to which it is equivalent, the\ntype of the expression is either `Standard.Boolean', or any type derived\nfrom this standard type.\n\nAssert checks can be either checked or ignored. By default they are\nignored.  They will be checked if either the command line switch\n`-gnata' is used, or if an `Assertion_Policy' or `Check_Policy' pragma\nis used to enable `Assert_Checks'.\n\nIf assertions are ignored, then there is no run-time effect (and in\nparticular, any side effects from the expression will not occur at run\ntime).  (The expression is still analyzed at compile time, and may\ncause types to be frozen if they are mentioned here for the first time).\n\nIf assertions are checked, then the given expression is tested, and if\nit is `False' then `System.Assertions.Raise_Assert_Failure' is called\nwhich results in the raising of `Assert_Failure' with the given message.\n\nYou should generally avoid side effects in the expression arguments of\nthis pragma, because these side effects will turn on and off with the\nsetting of the assertions mode, resulting in assertions that have an\neffect on the program.  However, the expressions are analyzed for\nsemantic correctness whether or not assertions are enabled, so turning\nassertions on and off cannot affect the legality of a program.\n\nNote that the implementation defined policy `DISABLE', given in a\npragma `Assertion_Policy', can be used to suppress this semantic\nanalysis.\n\nNote: this is a standard language-defined pragma in versions of Ada\nfrom 2005 on. In GNAT, it is implemented in all versions of Ada, and\nthe DISABLE policy is an implementation-defined addition.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Assert""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Assert_And_Cut (\nboolean_EXPRESSION\n[, string_EXPRESSION]);\n\nThe effect of this pragma is identical to that of pragma `Assert',\nexcept that in an `Assertion_Policy' pragma, the identifier\n`Assert_And_Cut' is used to control whether it is ignored or checked\n(or disabled).\n\nThe intention is that this be used within a subprogram when the given\ntest expresion sums up all the work done so far in the subprogram, so\nthat the rest of the subprogram can be verified (informally or\nformally) using only the entry preconditions, and the expression in\nthis pragma. This allows dividing up a subprogram into sections for the\npurposes of testing or formal verification. The pragma also serves as\nuseful documentation.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Assert_And_Cut""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Assertion_Policy (CHECK | DISABLE | IGNORE | SUPPRESSIBLE);\n\npragma Assertion_Policy (\nASSERTION_KIND => POLICY_IDENTIFIER\n{, ASSERTION_KIND => POLICY_IDENTIFIER});\n\nASSERTION_KIND ::= RM_ASSERTION_KIND | ID_ASSERTION_KIND\n\nRM_ASSERTION_KIND ::= Assert               |\nStatic_Predicate     |\nDynamic_Predicate    |\nPre                  |\nPre'Class            |\nPost                 |\nPost'Class           |\nType_Invariant       |\nType_Invariant'Class\n\nID_ASSERTION_KIND ::= Assertions           |\nAssert_And_Cut       |\nAssume               |\nContract_Cases       |\nDebug                |\nGhost                |\nInvariant            |\nInvariant'Class      |\nLoop_Invariant       |\nLoop_Variant         |\nPostcondition        |\nPrecondition         |\nPredicate            |\nRefined_Post         |\nStatement_Assertions\n\nPOLICY_IDENTIFIER ::= Check | Disable | Ignore | Suppressible\n\nThis is a standard Ada 2012 pragma that is available as an\nimplementation-defined pragma in earlier versions of Ada.  The\nassertion kinds `RM_ASSERTION_KIND' are those defined in the Ada\nstandard. The assertion kinds `ID_ASSERTION_KIND' are implementation\ndefined additions recognized by the GNAT compiler.\n\nThe pragma applies in both cases to pragmas and aspects with matching\nnames, e.g. `Pre' applies to the Pre aspect, and `Precondition' applies\nto both the `Precondition' pragma and the aspect `Precondition'. Note\nthat the identifiers for pragmas Pre_Class and Post_Class are Pre'Class\nand Post'Class (not Pre_Class and Post_Class), since these pragmas are\nintended to be identical to the corresponding aspects).\n\nIf the policy is `CHECK', then assertions are enabled, i.e.  the\ncorresponding pragma or aspect is activated.  If the policy is\n`IGNORE', then assertions are ignored, i.e.  the corresponding pragma\nor aspect is deactivated.  This pragma overrides the effect of the\n`-gnata' switch on the command line.  If the policy is `SUPPRESSIBLE',\nthen assertions are enabled by default, however, if the `-gnatp' switch\nis specified all assertions are ignored.\n\nThe implementation defined policy `DISABLE' is like `IGNORE' except\nthat it completely disables semantic checking of the corresponding\npragma or aspect. This is useful when the pragma or aspect argument\nreferences subprograms in a with'ed package which is replaced by a\ndummy package for the final build.\n\nThe implementation defined assertion kind `Assertions' applies to all\nassertion kinds. The form with no assertion kind given implies this\nchoice, so it applies to all assertion kinds (RM defined, and\nimplementation defined).\n\nThe implementation defined assertion kind `Statement_Assertions'\napplies to `Assert', `Assert_And_Cut', `Assume', `Loop_Invariant', and\n`Loop_Variant'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Assertion_Policy""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Assume (\nboolean_EXPRESSION\n[, string_EXPRESSION]);\n\nThe effect of this pragma is identical to that of pragma `Assert',\nexcept that in an `Assertion_Policy' pragma, the identifier `Assume' is\nused to control whether it is ignored or checked (or disabled).\n\nThe intention is that this be used for assumptions about the external\nenvironment. So you cannot expect to verify formally or informally that\nthe condition is met, this must be established by examining things\noutside the program itself.  For example, we may have code that depends\non the size of `Long_Long_Integer' being at least 64. So we could write:\n\npragma Assume (Long_Long_Integer'Size >= 64);\n\nThis assumption cannot be proved from the program itself, but it acts\nas a useful run-time check that the assumption is met, and documents\nthe need to ensure that it is met by reference to information outside\nthe program.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Assume""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Assume_No_Invalid_Values (On | Off);\n\nThis is a configuration pragma that controls the assumptions made by the\ncompiler about the occurrence of invalid representations (invalid\nvalues) in the code.\n\nThe default behavior (corresponding to an Off argument for this\npragma), is to assume that values may in general be invalid unless the\ncompiler can prove they are valid. Consider the following example:\n\nV1 : Integer range 1 .. 10;\nV2 : Integer range 11 .. 20;\n\nfor J in V2 .. V1 loop\n\nend loop;\n\nif V1 and V2 have valid values, then the loop is known at compile time\nnot to execute since the lower bound must be greater than the upper\nbound. However in default mode, no such assumption is made, and the\nloop may execute. If `Assume_No_Invalid_Values (On)' is given, the\ncompiler will assume that any occurrence of a variable other than in an\nexplicit `'Valid' test always has a valid value, and the loop above\nwill be optimized away.\n\nThe use of `Assume_No_Invalid_Values (On)' is appropriate if you know\nyour code is free of uninitialized variables and other possible sources\nof invalid representations, and may result in more efficient code. A\nprogram that accesses an invalid representation with this pragma in\neffect is erroneous, so no guarantees can be made about its behavior.\n\nIt is peculiar though permissible to use this pragma in conjunction\nwith validity checking (-gnatVa). In such cases, accessing invalid\nvalues will generally give an exception, though formally the program is\nerroneous so there are no guarantees that this will always be the case,\nand it is recommended that these two options not be used together.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Assume_No_Invalid_Values""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Async_Readers [ (boolean_EXPRESSION) ];\n\nFor the semantics of this pragma, see the entry for aspect\n`Async_Readers' in the SPARK 2014 Reference Manual, section 7.1.2.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Async_Readers""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Async_Writers [ (boolean_EXPRESSION) ];\n\nFor the semantics of this pragma, see the entry for aspect\n`Async_Writers' in the SPARK 2014 Reference Manual, section 7.1.2.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Async_Writers""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Asynchronous;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Asynchronous""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Atomic;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Atomic""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Atomic_Components;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Atomic_Components""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Attach_Handler;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Attach_Handler""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Attribute_Definition\n([Attribute  =>] ATTRIBUTE_DESIGNATOR,\n[Entity     =>] LOCAL_NAME,\n[Expression =>] EXPRESSION | NAME);\n\nIf `Attribute' is a known attribute name, this pragma is equivalent to\nthe attribute definition clause:\n\nfor Entity'Attribute use Expression;\n\nIf `Attribute' is not a recognized attribute name, the pragma is\nignored, and a warning is emitted. This allows source code to be\nwritten that takes advantage of some new attribute, while remaining\ncompilable with earlier compilers.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Attribute_Definition""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma CPP_Class ([Entity =>] LOCAL_NAME);\n\nThe argument denotes an entity in the current declarative region that is\ndeclared as a record type. It indicates that the type corresponds to an\nexternally declared C++ class type, and is to be laid out the same way\nthat C++ would lay out the type. If the C++ class has virtual primitives\nthen the record must be declared as a tagged record type.\n\nTypes for which `CPP_Class' is specified do not have assignment or\nequality operators defined (such operations can be imported or declared\nas subprograms as required). Initialization is allowed only by\nconstructor functions (see pragma `CPP_Constructor'). Such types are\nimplicitly limited if not explicitly declared as limited or derived\nfrom a limited type, and an error is issued in that case.\n\nSee *note Interfacing to C++: 4a. for related information.\n\nNote: Pragma `CPP_Class' is currently obsolete. It is supported for\nbackward compatibility but its functionality is available using pragma\n`Import' with `Convention' = `CPP'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""CPP_Class""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma CPP_Constructor ([Entity =>] LOCAL_NAME\n[, [External_Name =>] static_string_EXPRESSION ]\n[, [Link_Name     =>] static_string_EXPRESSION ]);\n\nThis pragma identifies an imported function (imported in the usual way\nwith pragma `Import') as corresponding to a C++ constructor. If\n`External_Name' and `Link_Name' are not specified then the `Entity'\nargument is a name that must have been previously mentioned in a pragma\n`Import' with `Convention' = `CPP'. Such name must be of one of the\nfollowing forms:\n\n* `function' `Fname' `return' T`\n\n* `function' `Fname' `return' T'Class\n\n* `function' `Fname' (...) `return' T`\n\n* `function' `Fname' (...) `return' T'Class\n\nwhere `T' is a limited record type imported from C++ with pragma\n`Import' and `Convention' = `CPP'.\n\nThe first two forms import the default constructor, used when an object\nof type `T' is created on the Ada side with no explicit constructor.\nThe latter two forms cover all the non-default constructors of the type.\nSee the GNAT User's Guide for details.\n\nIf no constructors are imported, it is impossible to create any objects\non the Ada side and the type is implicitly declared abstract.\n\nPragma `CPP_Constructor' is intended primarily for automatic generation\nusing an automatic binding generator tool (such as the `-fdump-ada-spec'\nGCC switch).  See *note Interfacing to C++: 4a. for more related\ninformation.\n\nNote: The use of functions returning class-wide types for constructors\nis currently obsolete. They are supported for backward compatibility.\nThe use of functions returning the type T leave the Ada sources more\nclear because the imported C++ constructors always return an object of\ntype T; that is, they never return an object whose type is a descendant\nof type T.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""CPP_Constructor""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This pragma is now obsolete and, other than generating a warning if\nwarnings on obsolescent features are enabled, is completely ignored.\nIt is retained for compatibility purposes. It used to be required to\nensure compoatibility with C++, but is no longer required for that\npurpose because GNAT generates the same object layout as the G++\ncompiler by default.\n\nSee *note Interfacing to C++: 4a. for related information.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""CPP_Virtual""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This pragma is now obsolete and, other than generating a warning if\nwarnings on obsolescent features are enabled, is completely ignored.\nIt used to be required to ensure compatibility with C++, but is no\nlonger required for that purpose because GNAT generates the same object\nlayout as the G++ compiler by default.\n\nSee *note Interfacing to C++: 4a. for related information.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""CPP_Vtable""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma CPU (EXPRESSION);\n\nThis pragma is standard in Ada 2012, but is available in all earlier\nversions of Ada as an implementation-defined pragma.  See Ada 2012\nReference Manual for details.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""CPU""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma C_Pass_By_Copy\n([Max_Size =>] static_integer_EXPRESSION);\n\nNormally the default mechanism for passing C convention records to C\nconvention subprograms is to pass them by reference, as suggested by RM\nB.3(69).  Use the configuration pragma `C_Pass_By_Copy' to change this\ndefault, by requiring that record formal parameters be passed by copy\nif all of the following conditions are met:\n\n* The size of the record type does not exceed the value specified for\n`Max_Size'.\n\n* The record type has `Convention C'.\n\n* The formal parameter has this record type, and the subprogram has a\nforeign (non-Ada) convention.\n\nIf these conditions are met the argument is passed by copy; i.e., in a\nmanner consistent with what C expects if the corresponding formal in the\nC prototype is a struct (rather than a pointer to a struct).\n\nYou can also pass records by copy by specifying the convention\n`C_Pass_By_Copy' for the record type, or by using the extended `Import'\nand `Export' pragmas, which allow specification of passing mechanisms\non a parameter by parameter basis.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""C_Pass_By_Copy""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Check (\n[Name    =>] CHECK_KIND,\n[Check   =>] Boolean_EXPRESSION\n[, [Message =>] string_EXPRESSION] );\n\nCHECK_KIND ::= IDENTIFIER           |\nPre'Class            |\nPost'Class           |\nType_Invariant'Class |\nInvariant'Class\n\nThis pragma is similar to the predefined pragma `Assert' except that an\nextra identifier argument is present. In conjunction with pragma\n`Check_Policy', this can be used to define groups of assertions that can\nbe independently controlled. The identifier `Assertion' is special, it\nrefers to the normal set of pragma `Assert' statements.\n\nChecks introduced by this pragma are normally deactivated by default.\nThey can be activated either by the command line option `-gnata', which\nturns on all checks, or individually controlled using pragma\n`Check_Policy'.\n\nThe identifiers `Assertions' and `Statement_Assertions' are not\npermitted as check kinds, since this would cause confusion with the use\nof these identifiers in `Assertion_Policy' and `Check_Policy' pragmas,\nwhere they are used to refer to sets of assertions.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Check""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Check_Float_Overflow;\n\nIn Ada, the predefined floating-point types (`Short_Float', `Float',\n`Long_Float', `Long_Long_Float') are defined to be `unconstrained'.\nThis means that even though each has a well-defined base range, an\noperation that delivers a result outside this base range is not\nrequired to raise an exception.  This implementation permission\naccommodates the notion of infinities in IEEE floating-point, and\ncorresponds to the efficient execution mode on most machines. GNAT will\nnot raise overflow exceptions on these machines; instead it will\ngenerate infinities and NaN's as defined in the IEEE standard.\n\nGenerating infinities, although efficient, is not always desirable.\nOften the preferable approach is to check for overflow, even at the\n(perhaps considerable) expense of run-time performance.  This can be\naccomplished by defining your own constrained floating-point subtypes -\ni.e., by supplying explicit range constraints - and indeed such a\nsubtype can have the same base range as its base type. For example:\n\nsubtype My_Float is Float range Float'Range;\n\nHere `My_Float' has the same range as `Float' but is constrained, so\noperations on `My_Float' values will be checked for overflow against\nthis range.\n\nThis style will achieve the desired goal, but it is often more\nconvenient to be able to simply use the standard predefined\nfloating-point types as long as overflow checking could be guaranteed.\nThe `Check_Float_Overflow' configuration pragma achieves this effect.\nIf a unit is compiled subject to this configuration pragma, then all\noperations on predefined floating-point types including operations on\nbase types of these floating-point types will be treated as though\nthose types were constrained, and overflow checks will be generated.\nThe `Constraint_Error' exception is raised if the result is out of\nrange.\n\nThis mode can also be set by use of the compiler switch `-gnateF'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Check_Float_Overflow""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Check_Name (check_name_IDENTIFIER);\n\nThis is a configuration pragma that defines a new implementation\ndefined check name (unless IDENTIFIER matches one of the predefined\ncheck names, in which case the pragma has no effect). Check names are\nglobal to a partition, so if two or more configuration pragmas are\npresent in a partition mentioning the same name, only one new check\nname is introduced.\n\nAn implementation defined check name introduced with this pragma may be\nused in only three contexts: `pragma Suppress', `pragma Unsuppress',\nand as the prefix of a `Check_Name'Enabled' attribute reference. For\nany of these three cases, the check name must be visible. A check name\nis visible if it is in the configuration pragmas applying to the\ncurrent unit, or if it appears at the start of any unit that is part of\nthe dependency set of the current unit (e.g., units that are mentioned\nin `with' clauses).\n\nCheck names introduced by this pragma are subject to control by compiler\nswitches (in particular -gnatp) in the usual manner.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Check_Name""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Check_Policy\n([Name   =>] CHECK_KIND,\n[Policy =>] POLICY_IDENTIFIER);\n\npragma Check_Policy (\nCHECK_KIND => POLICY_IDENTIFIER\n{, CHECK_KIND => POLICY_IDENTIFIER});\n\nASSERTION_KIND ::= RM_ASSERTION_KIND | ID_ASSERTION_KIND\n\nCHECK_KIND ::= IDENTIFIER           |\nPre'Class            |\nPost'Class           |\nType_Invariant'Class |\nInvariant'Class\n\nThe identifiers Name and Policy are not allowed as CHECK_KIND values. This\navoids confusion between the two possible syntax forms for this pragma.\n\nPOLICY_IDENTIFIER ::= ON | OFF | CHECK | DISABLE | IGNORE\n\nThis pragma is used to set the checking policy for assertions (specified\nby aspects or pragmas), the `Debug' pragma, or additional checks to be\nchecked using the `Check' pragma. It may appear either as a\nconfiguration pragma, or within a declarative part of package. In the\nlatter case, it applies from the point where it appears to the end of\nthe declarative region (like pragma `Suppress').\n\nThe `Check_Policy' pragma is similar to the predefined\n`Assertion_Policy' pragma, and if the check kind corresponds to one of\nthe assertion kinds that are allowed by `Assertion_Policy', then the\neffect is identical.\n\nIf the first argument is Debug, then the policy applies to Debug\npragmas, disabling their effect if the policy is `OFF', `DISABLE', or\n`IGNORE', and allowing them to execute with normal semantics if the\npolicy is `ON' or `CHECK'. In addition if the policy is `DISABLE', then\nthe procedure call in `Debug' pragmas will be totally ignored and not\nanalyzed semantically.\n\nFinally the first argument may be some other identifier than the above\npossibilities, in which case it controls a set of named assertions that\ncan be checked using pragma `Check'. For example, if the pragma:\n\npragma Check_Policy (Critical_Error, OFF);\n\nis given, then subsequent `Check' pragmas whose first argument is also\n`Critical_Error' will be disabled.\n\nThe check policy is `OFF' to turn off corresponding checks, and `ON' to\nturn on corresponding checks. The default for a set of checks for which\nno `Check_Policy' is given is `OFF' unless the compiler switch `-gnata'\nis given, which turns on all checks by default.\n\nThe check policy settings `CHECK' and `IGNORE' are recognized as\nsynonyms for `ON' and `OFF'. These synonyms are provided for\ncompatibility with the standard `Assertion_Policy' pragma. The check\npolicy setting `DISABLE' causes the second argument of a corresponding\n`Check' pragma to be completely ignored and not analyzed.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Check_Policy""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Comment (static_string_EXPRESSION);\n\nThis is almost identical in effect to pragma `Ident'.  It allows the\nplacement of a comment into the object file and hence into the\nexecutable file if the operating system permits such usage.  The\ndifference is that `Comment', unlike `Ident', has no limitations on\nplacement of the pragma (it can be placed anywhere in the main source\nunit), and if more than one pragma is used, all comments are retained.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Comment""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Common_Object (\n[Internal =>] LOCAL_NAME\n[, [External =>] EXTERNAL_SYMBOL]\n[, [Size     =>] EXTERNAL_SYMBOL] );\n\nEXTERNAL_SYMBOL ::=\nIDENTIFIER\n| static_string_EXPRESSION\n\nThis pragma enables the shared use of variables stored in overlaid\nlinker areas corresponding to the use of `COMMON' in Fortran.  The\nsingle object `LOCAL_NAME' is assigned to the area designated by the\n`External' argument.  You may define a record to correspond to a series\nof fields.  The `Size' argument is syntax checked in GNAT, but\notherwise ignored.\n\n`Common_Object' is not supported on all platforms.  If no support is\navailable, then the code generator will issue a message indicating that\nthe necessary attribute for implementation of this pragma is not\navailable.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Common_Object""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Compile_Time_Error\n(boolean_EXPRESSION, static_string_EXPRESSION);\n\nThis pragma can be used to generate additional compile time error\nmessages. It is particularly useful in generics, where errors can be\nissued for specific problematic instantiations. The first parameter is\na boolean expression. The pragma is effective only if the value of this\nexpression is known at compile time, and has the value True. The set of\nexpressions whose values are known at compile time includes all static\nboolean expressions, and also other values which the compiler can\ndetermine at compile time (e.g., the size of a record type set by an\nexplicit size representation clause, or the value of a variable which\nwas initialized to a constant and is known not to have been modified).\nIf these conditions are met, an error message is generated using the\nvalue given as the second argument. This string value may contain\nembedded ASCII.LF characters to break the message into multiple lines.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Compile_Time_Error""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Compile_Time_Warning\n(boolean_EXPRESSION, static_string_EXPRESSION);\n\nSame as pragma Compile_Time_Error, except a warning is issued instead\nof an error message. Note that if this pragma is used in a package that\nis with'ed by a client, the client will get the warning even though it\nis issued by a with'ed package (normally warnings in with'ed units are\nsuppressed, but this is a special exception to that rule).\n\nOne typical use is within a generic where compile time known\ncharacteristics of formal parameters are tested, and warnings given\nappropriately. Another use with a first parameter of True is to warn a\nclient about use of a package, for example that it is not fully\nimplemented.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Compile_Time_Warning""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Compiler_Unit;\n\nThis pragma is obsolete. It is equivalent to Compiler_Unit_Warning. It\nis retained so that old versions of the GNAT run-time that use this\npragma can be compiled with newer versions of the compiler.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Compiler_Unit""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Compiler_Unit_Warning;\n\nThis pragma is intended only for internal use in the GNAT run-time\nlibrary.  It indicates that the unit is used as part of the compiler\nbuild. The effect is to generate warnings for the use of constructs\n(for example, conditional expressions) that would cause trouble when\nbootstrapping using an older version of GNAT. For the exact list of\nrestrictions, see the compiler sources and references to\nCheck_Compiler_Unit.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Compiler_Unit_Warning""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Complete_Representation;\n\nThis pragma must appear immediately within a record representation\nclause. Typical placements are before the first component clause or\nafter the last component clause. The effect is to give an error message\nif any component is missing a component clause. This pragma may be used\nto ensure that a record representation clause is complete, and that\nthis invariant is maintained if fields are added to the record in the\nfuture.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Complete_Representation""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Complex_Representation\n([Entity =>] LOCAL_NAME);\n\nThe `Entity' argument must be the name of a record type which has two\nfields of the same floating-point type.  The effect of this pragma is\nto force gcc to use the special internal complex representation form for\nthis record, which may be more efficient.  Note that this may result in\nthe code for this type not conforming to standard ABI (application\nbinary interface) requirements for the handling of record types.  For\nexample, in some environments, there is a requirement for passing\nrecords by pointer, and the use of this pragma may result in passing\nthis type in floating-point registers.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Complex_Representation""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Component_Alignment (\n[Form =>] ALIGNMENT_CHOICE\n[, [Name =>] type_LOCAL_NAME]);\n\nALIGNMENT_CHOICE ::=\nComponent_Size\n| Component_Size_4\n| Storage_Unit\n| Default\n\nSpecifies the alignment of components in array or record types.  The\nmeaning of the `Form' argument is as follows:\n\n\n`Component_Size'\nAligns scalar components and subcomponents of the array or record\ntype on boundaries appropriate to their inherent size (naturally\naligned).  For example, 1-byte components are aligned on byte\nboundaries, 2-byte integer components are aligned on 2-byte\nboundaries, 4-byte integer components are aligned on 4-byte\nboundaries and so on.  These alignment rules correspond to the\nnormal rules for C compilers on all machines except the VAX.\n\n`Component_Size_4'\nNaturally aligns components with a size of four or fewer bytes.\nComponents that are larger than 4 bytes are placed on the next\n-byte boundary.\n\n`Storage_Unit'\nSpecifies that array or record components are byte aligned, i.e.,\naligned on boundaries determined by the value of the constant\n`System.Storage_Unit'.\n\n`Default'\nSpecifies that array or record components are aligned on default\nboundaries, appropriate to the underlying hardware or operating\nsystem or both. The `Default' choice is the same as\n`Component_Size' (natural alignment).\n\nIf the `Name' parameter is present, `type_LOCAL_NAME' must refer to a\nlocal record or array type, and the specified alignment choice applies\nto the specified type.  The use of `Component_Alignment' together with\na pragma `Pack' causes the `Component_Alignment' pragma to be ignored.\nThe use of `Component_Alignment' together with a record representation\nclause is only effective for fields not specified by the representation\nclause.\n\nIf the `Name' parameter is absent, the pragma can be used as either a\nconfiguration pragma, in which case it applies to one or more units in\naccordance with the normal rules for configuration pragmas, or it can be\nused within a declarative part, in which case it applies to types that\nare declared within this declarative part, or within any nested scope\nwithin this declarative part.  In either case it specifies the alignment\nto be applied to any record or array type which has otherwise standard\nrepresentation.\n\nIf the alignment for a record or array type is not specified (using\npragma `Pack', pragma `Component_Alignment', or a record rep clause),\nthe GNAT uses the default alignment as described previously.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Component_Alignment""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Constant_After_Elaboration [ (boolean_EXPRESSION) ];\n\nFor the semantics of this pragma, see the entry for aspect\n`Constant_After_Elaboration' in the SPARK 2014 Reference Manual,\nsection 3.3.1.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Constant_After_Elaboration""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Contract_Cases ((CONTRACT_CASE {, CONTRACT_CASE));\n\nCONTRACT_CASE ::= CASE_GUARD => CONSEQUENCE\n\nCASE_GUARD ::= boolean_EXPRESSION | others\n\nCONSEQUENCE ::= boolean_EXPRESSION\n\nThe `Contract_Cases' pragma allows defining fine-grain specifications\nthat can complement or replace the contract given by a precondition and\na postcondition. Additionally, the `Contract_Cases' pragma can be used\nby testing and formal verification tools. The compiler checks its\nvalidity and, depending on the assertion policy at the point of\ndeclaration of the pragma, it may insert a check in the executable. For\ncode generation, the contract cases\n\npragma Contract_Cases (\nCond1 => Pred1,\nCond2 => Pred2);\n\nare equivalent to\n\nC1 : constant Boolean := Cond1;  --  evaluated at subprogram entry\nC2 : constant Boolean := Cond2;  --  evaluated at subprogram entry\npragma Precondition ((C1 and not C2) or (C2 and not C1));\npragma Postcondition (if C1 then Pred1);\npragma Postcondition (if C2 then Pred2);\n\nThe precondition ensures that one and only one of the case guards is\nsatisfied on entry to the subprogram.  The postcondition ensures that\nfor the case guard that was True on entry, the corrresponding\nconsequence is True on exit. Other consequence expressions are not\nevaluated.\n\nA precondition `P' and postcondition `Q' can also be expressed as\ncontract cases:\n\npragma Contract_Cases (P => Q);\n\nThe placement and visibility rules for `Contract_Cases' pragmas are\nidentical to those described for preconditions and postconditions.\n\nThe compiler checks that boolean expressions given in case guards and\nconsequences are valid, where the rules for case guards are the same as\nthe rule for an expression in `Precondition' and the rules for\nconsequences are the same as the rule for an expression in\n`Postcondition'. In particular, attributes `'Old' and `'Result' can\nonly be used within consequence expressions.  The case guard for the\nlast contract case may be `others', to denote any case not captured by\nthe previous cases. The following is an example of use within a package\nspec:\n\npackage Math_Functions is\n\nfunction Sqrt (Arg : Float) return Float;\npragma Contract_Cases (((Arg in 0.0 .. 99.0) => Sqrt'Result < 10.0,\nArg >= 100.0         => Sqrt'Result >= 10.0,\nothers               => Sqrt'Result = 0.0));\n\nend Math_Functions;\n\nThe meaning of contract cases is that only one case should apply at each\ncall, as determined by the corresponding case guard evaluating to True,\nand that the consequence for this case should hold when the subprogram\nreturns.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Contract_Cases""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Convention ([Convention=>]convention_identifier,[Entity=>]local_name);""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Convention""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Convention_Identifier (\n[Name =>]       IDENTIFIER,\n[Convention =>] convention_IDENTIFIER);\n\nThis pragma provides a mechanism for supplying synonyms for existing\nconvention identifiers. The `Name' identifier can subsequently be used\nas a synonym for the given convention in other pragmas (including for\nexample pragma `Import' or another `Convention_Identifier' pragma). As\nan example of the use of this, suppose you had legacy code which used\nFortran77 as the identifier for Fortran. Then the pragma:\n\npragma Convention_Identifier (Fortran77, Fortran);\n\nwould allow the use of the convention identifier `Fortran77' in\nsubsequent code, avoiding the need to modify the sources. As another\nexample, you could use this to parameterize convention requirements\naccording to systems. Suppose you needed to use `Stdcall' on windows\nsystems, and `C' on some other system, then you could define a\nconvention identifier `Library' and use a single\n`Convention_Identifier' pragma to specify which convention would be\nused system-wide.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Convention_Identifier""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Deadline_Floor (time_span_EXPRESSION);\n\nThis pragma applies only to protected types and specifies the floor\ndeadline inherited by a task when the task enters a protected object.\nIt is effective only when the EDF scheduling policy is used.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Deadline_Floor""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Debug ([CONDITION, ]PROCEDURE_CALL_WITHOUT_SEMICOLON);\n\nPROCEDURE_CALL_WITHOUT_SEMICOLON ::=\nPROCEDURE_NAME\n| PROCEDURE_PREFIX ACTUAL_PARAMETER_PART\n\nThe procedure call argument has the syntactic form of an expression,\nmeeting the syntactic requirements for pragmas.\n\nIf debug pragmas are not enabled or if the condition is present and\nevaluates to False, this pragma has no effect. If debug pragmas are\nenabled, the semantics of the pragma is exactly equivalent to the\nprocedure call statement corresponding to the argument with a\nterminating semicolon. Pragmas are permitted in sequences of\ndeclarations, so you can use pragma `Debug' to intersperse calls to\ndebug procedures in the middle of declarations. Debug pragmas can be\nenabled either by use of the command line switch `-gnata' or by use of\nthe pragma `Check_Policy' with a first argument of `Debug'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Debug""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Debug_Policy (CHECK | DISABLE | IGNORE | ON | OFF);\n\nThis pragma is equivalent to a corresponding `Check_Policy' pragma with\na first argument of `Debug'. It is retained for historical\ncompatibility reasons.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Debug_Policy""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Default_Initial_Condition [ (null | boolean_EXPRESSION) ];\n\nFor the semantics of this pragma, see the entry for aspect\n`Default_Initial_Condition' in the SPARK 2014 Reference Manual, section\n7.3.3.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Default_Initial_Condition""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Default_Scalar_Storage_Order (High_Order_First | Low_Order_First);\n\nNormally if no explicit `Scalar_Storage_Order' is given for a record\ntype or array type, then the scalar storage order defaults to the\nordinary default for the target. But this default may be overridden\nusing this pragma.  The pragma may appear as a configuration pragma, or\nlocally within a package spec or declarative part. In the latter case,\nit applies to all subsequent types declared within that package spec or\ndeclarative part.\n\nThe following example shows the use of this pragma:\n\npragma Default_Scalar_Storage_Order (High_Order_First);\nwith System; use System;\npackage DSSO1 is\ntype H1 is record\na : Integer;\nend record;\n\ntype L2 is record\na : Integer;\nend record;\nfor L2'Scalar_Storage_Order use Low_Order_First;\n\ntype L2a is new L2;\n\npackage Inner is\ntype H3 is record\na : Integer;\nend record;\n\npragma Default_Scalar_Storage_Order (Low_Order_First);\n\ntype L4 is record\na : Integer;\nend record;\nend Inner;\n\ntype H4a is new Inner.L4;\n\ntype H5 is record\na : Integer;\nend record;\nend DSSO1;\n\nIn this example record types with names starting with `L' have\n`Low_Order_First' scalar storage order, and record types with names\nstarting with `H' have `High_Order_First'.  Note that in the case of\n`H4a', the order is not inherited from the parent type. Only an\nexplicitly set `Scalar_Storage_Order' gets inherited on type derivation.\n\nIf this pragma is used as a configuration pragma which appears within a\nconfiguration pragma file (as opposed to appearing explicitly at the\nstart of a single unit), then the binder will require that all units in\na partition be compiled in a similar manner, other than run-time units,\nwhich are not affected by this pragma. Note that the use of this form\nis discouraged because it may significantly degrade the run-time\nperformance of the software, instead the default scalar storage order\nought to be changed only on a local basis.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Default_Scalar_Storage_Order""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Default_Storage_Pool (storage_pool_NAME | null);\n\nThis pragma is standard in Ada 2012, but is available in all earlier\nversions of Ada as an implementation-defined pragma.  See Ada 2012\nReference Manual for details.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Default_Storage_Pool""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Depends (DEPENDENCY_RELATION);\n\nDEPENDENCY_RELATION ::=\nnull\n| (DEPENDENCY_CLAUSE {, DEPENDENCY_CLAUSE})\n\nDEPENDENCY_CLAUSE ::=\nOUTPUT_LIST =>[+] INPUT_LIST\n| NULL_DEPENDENCY_CLAUSE\n\nNULL_DEPENDENCY_CLAUSE ::= null => INPUT_LIST\n\nOUTPUT_LIST ::= OUTPUT | (OUTPUT {, OUTPUT})\n\nINPUT_LIST ::= null | INPUT | (INPUT {, INPUT})\n\nOUTPUT ::= NAME | FUNCTION_RESULT\nINPUT  ::= NAME\n\nwhere FUNCTION_RESULT is a function Result attribute_reference\n\nFor the semantics of this pragma, see the entry for aspect `Depends' in\nthe SPARK 2014 Reference Manual, section 6.1.5.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Depends""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Detect_Blocking;\n\nThis is a standard pragma in Ada 2005, that is available in all earlier\nversions of Ada as an implementation-defined pragma.\n\nThis is a configuration pragma that forces the detection of potentially\nblocking operations within a protected operation, and to raise\nProgram_Error if that happens.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Detect_Blocking""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Disable_Atomic_Synchronization [(Entity)];\n\nAda requires that accesses (reads or writes) of an atomic variable be\nregarded as synchronization points in the case of multiple tasks.\nParticularly in the case of multi-processors this may require special\nhandling, e.g. the generation of memory barriers. This capability may\nbe turned off using this pragma in cases where it is known not to be\nrequired.\n\nThe placement and scope rules for this pragma are the same as those for\n`pragma Suppress'. In particular it can be used as a configuration\npragma, or in a declaration sequence where it applies till the end of\nthe scope. If an `Entity' argument is present, the action applies only\nto that entity.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Disable_Atomic_Synchronization""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Discard_Names [([On => ] local_name)];""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Discard_Names""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Dispatching_Domain (EXPRESSION);\n\nThis pragma is standard in Ada 2012, but is available in all earlier\nversions of Ada as an implementation-defined pragma.  See Ada 2012\nReference Manual for details.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Dispatching_Domain""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Effective_Reads [ (boolean_EXPRESSION) ];\n\nFor the semantics of this pragma, see the entry for aspect\n`Effective_Reads' in the SPARK 2014 Reference Manual, section 7.1.2.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Effective_Reads""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Effective_Writes [ (boolean_EXPRESSION) ];\n\nFor the semantics of this pragma, see the entry for aspect\n`Effective_Writes' in the SPARK 2014 Reference Manual, section 7.1.2.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Effective_Writes""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Elaborate (library_unit_name{, library_unit_name});""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Elaborate""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Elaborate_All (library_unit_name{, library_unit_name});""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Elaborate_All""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Elaborate_Body [(library_unit_name)];""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Elaborate_Body""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Elaboration_Checks (Dynamic | Static);\n\nThis is a configuration pragma which specifies the elaboration model to\nbe used during compilation. For more information on the elaboration\nmodels of GNAT, consult the chapter on elaboration order handling in\nthe `GNAT User's Guide'.\n\nThe pragma may appear in the following contexts:\n\n* Configuration pragmas file\n\n* Prior to the context clauses of a compilation unit's initial\ndeclaration\n\nAny other placement of the pragma will result in a warning and the\neffects of the offending pragma will be ignored.\n\nIf the pragma argument is `Dynamic', then the dynamic elaboration model\nis in effect. If the pragma argument is `Static', then the static\nelaboration model is in effect.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Elaboration_Checks""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Eliminate (\n[  Unit_Name       => ] IDENTIFIER | SELECTED_COMPONENT ,\n[  Entity          => ] IDENTIFIER |\nSELECTED_COMPONENT |\nSTRING_LITERAL\n[, Source_Location =>   SOURCE_TRACE ] );\n\nSOURCE_TRACE    ::= STRING_LITERAL\n\nThis pragma indicates that the given entity is not used in the program\nto be compiled and built, thus allowing the compiler to eliminate the\ncode or data associated with the named entity. Any reference to an\neliminated entity causes a compile-time or link-time error.\n\nThe pragma has the following semantics, where `U' is the unit specified\nby the `Unit_Name' argument and `E' is the entity specified by the\n`Entity' argument:\n\n* `E' must be a subprogram that is explicitly declared either:\n\no  Within `U', or\n\no  Within a generic package that is instantiated in `U', or\n\no  As an instance of generic subprogram instantiated in `U'.\n\nOtherwise the pragma is ignored.\n\n* If `E' is overloaded within `U' then, in the absence of a\n`Source_Location' argument, all overloadings are eliminated.\n\n* If `E' is overloaded within `U' and only some overloadings are to\nbe eliminated, then each overloading to be eliminated must be\nspecified in a corresponding pragma `Eliminate' with a\n`Source_Location' argument identifying the line where the\ndeclaration appears, as described below.\n\n* If `E' is declared as the result of a generic instantiation, then\na `Source_Location' argument is needed, as described below\n\nPragma `Eliminate' allows a program to be compiled in a\nsystem-independent manner, so that unused entities are eliminated but\nwithout needing to modify the source text. Normally the required set of\n`Eliminate' pragmas is constructed automatically using the `gnatelim'\ntool.\n\nAny source file change that removes, splits, or adds lines may make the\nset of `Eliminate' pragmas invalid because their `Source_Location'\nargument values may get out of date.\n\nPragma `Eliminate' may be used where the referenced entity is a\ndispatching operation. In this case all the subprograms to which the\ngiven operation can dispatch are considered to be unused (are never\ncalled as a result of a direct or a dispatching call).\n\nThe string literal given for the source location specifies the line\nnumber of the declaration of the entity, using the following syntax for\n`SOURCE_TRACE':\n\nSOURCE_TRACE     ::= SOURCE_REFERENCE [ LBRACKET SOURCE_TRACE RBRACKET ]\n\nLBRACKET         ::= '['\nRBRACKET         ::= ']'\n\nSOURCE_REFERENCE ::= FILE_NAME : LINE_NUMBER\n\nLINE_NUMBER      ::= DIGIT {DIGIT}\n\nSpaces around the colon in a `SOURCE_REFERENCE' are optional.\n\nThe source trace that is given as the `Source_Location' must obey the\nfollowing rules (or else the pragma is ignored), where `U' is the unit\n`U' specified by the `Unit_Name' argument and `E' is the subprogram\nspecified by the `Entity' argument:\n\n* `FILE_NAME' is the short name (with no directory information) of\nthe Ada source file for `U', using the required syntax for the\nunderlying file system (e.g. case is significant if the underlying\noperating system is case sensitive).  If `U' is a package and `E'\nis a subprogram declared in the package specification and its full\ndeclaration appears in the package body, then the  relevant source\nfile is the one for the package specification; analogously if `U'\nis a generic package.\n\n* If `E' is not declared in a generic instantiation (this includes\ngeneric subprogram instances), the source trace includes only one\nsource line reference. `LINE_NUMBER' gives the line number of the\noccurrence of the declaration of `E' within the source file (as a\ndecimal literal without an exponent or point).\n\n* If `E' is declared by a generic instantiation, its source trace\n(from left to right) starts with the source location of the\ndeclaration of `E' in the generic unit and ends with the source\nlocation of the instantiation, given in square brackets. This\napproach is applied recursively with nested instantiations: the\nrightmost (nested most deeply in square brackets) element of the\nsource trace is the location of the outermost instantiation, and\nthe leftmost element (that is, outside of any square brackets) is\nthe location of the declaration of `E' in the generic unit.\n\nExamples:\n\npragma Eliminate (Pkg0, Proc);\n-- Eliminate (all overloadings of) Proc in Pkg0\n\npragma Eliminate (Pkg1, Proc,\nSource_Location => \""pkg1.ads:8\"");\n-- Eliminate overloading of Proc at line 8 in pkg1.ads\n\n-- Assume the following file contents:\n--   gen_pkg.ads\n--   1: generic\n--   2:   type T is private;\n--   3: package Gen_Pkg is\n--   4:   procedure Proc(N : T);\n--  ...   ...\n--  ... end Gen_Pkg;\n--\n--    q.adb\n--   1: with Gen_Pkg;\n--   2: procedure Q is\n--   3:   package Inst_Pkg is new Gen_Pkg(Integer);\n--  ...   -- No calls on Inst_Pkg.Proc\n--  ... end Q;\n\n-- The following pragma eliminates Inst_Pkg.Proc from Q\npragma Eliminate (Q, Proc,\nSource_Location => \""gen_pkg.ads:4[q.adb:3]\"");""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Eliminate""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Enable_Atomic_Synchronization [(Entity)];\n\nAda requires that accesses (reads or writes) of an atomic variable be\nregarded as synchronization points in the case of multiple tasks.\nParticularly in the case of multi-processors this may require special\nhandling, e.g. the generation of memory barriers. This synchronization\nis performed by default, but can be turned off using `pragma\nDisable_Atomic_Synchronization'. The `Enable_Atomic_Synchronization'\npragma can be used to turn it back on.\n\nThe placement and scope rules for this pragma are the same as those for\n`pragma Unsuppress'. In particular it can be used as a configuration\npragma, or in a declaration sequence where it applies till the end of\nthe scope. If an `Entity' argument is present, the action applies only\nto that entity.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Enable_Atomic_Synchronization""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Export ([Convention=>]convention_identifier,[Entity=>]local_name;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Export""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Export_Function (\n[Internal         =>] LOCAL_NAME\n[, [External         =>] EXTERNAL_SYMBOL]\n[, [Parameter_Types  =>] PARAMETER_TYPES]\n[, [Result_Type      =>] result_SUBTYPE_MARK]\n[, [Mechanism        =>] MECHANISM]\n[, [Result_Mechanism =>] MECHANISM_NAME]);\n\nEXTERNAL_SYMBOL ::=\nIDENTIFIER\n| static_string_EXPRESSION\n| \""\""\n\nPARAMETER_TYPES ::=\nnull\n| TYPE_DESIGNATOR {, TYPE_DESIGNATOR}\n\nTYPE_DESIGNATOR ::=\nsubtype_NAME\n| subtype_Name ' Access\n\nMECHANISM ::=\nMECHANISM_NAME\n| (MECHANISM_ASSOCIATION {, MECHANISM_ASSOCIATION})\n\nMECHANISM_ASSOCIATION ::=\n[formal_parameter_NAME =>] MECHANISM_NAME\n\nMECHANISM_NAME ::= Value | Reference\n\nUse this pragma to make a function externally callable and optionally\nprovide information on mechanisms to be used for passing parameter and\nresult values.  We recommend, for the purposes of improving portability,\nthis pragma always be used in conjunction with a separate pragma\n`Export', which must precede the pragma `Export_Function'.  GNAT does\nnot require a separate pragma `Export', but if none is present,\n`Convention Ada' is assumed, which is usually not what is wanted, so it\nis usually appropriate to use this pragma in conjunction with a\n`Export' or `Convention' pragma that specifies the desired foreign\nconvention.  Pragma `Export_Function' (and `Export', if present) must\nappear in the same declarative region as the function to which they\napply.\n\nThe `internal_name' must uniquely designate the function to which the\npragma applies.  If more than one function name exists of this name in\nthe declarative part you must use the `Parameter_Types' and\n`Result_Type' parameters to achieve the required unique designation.\nThe `subtype_mark's in these parameters must exactly match the subtypes\nin the corresponding function specification, using positional notation\nto match parameters with subtype marks.  The form with an `'Access'\nattribute can be used to match an anonymous access parameter.\n\nSpecial treatment is given if the EXTERNAL is an explicit null string\nor a static string expressions that evaluates to the null string. In\nthis case, no external name is generated. This form still allows the\nspecification of parameter mechanisms.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Export_Function""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Export_Object\n[Internal =>] LOCAL_NAME\n[, [External =>] EXTERNAL_SYMBOL]\n[, [Size     =>] EXTERNAL_SYMBOL]\n\nEXTERNAL_SYMBOL ::=\nIDENTIFIER\n| static_string_EXPRESSION\n\nThis pragma designates an object as exported, and apart from the\nextended rules for external symbols, is identical in effect to the use\nof the normal `Export' pragma applied to an object.  You may use a\nseparate Export pragma (and you probably should from the point of view\nof portability), but it is not required.  `Size' is syntax checked, but\notherwise ignored by GNAT.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Export_Object""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Export_Procedure (\n[Internal        =>] LOCAL_NAME\n[, [External        =>] EXTERNAL_SYMBOL]\n[, [Parameter_Types =>] PARAMETER_TYPES]\n[, [Mechanism       =>] MECHANISM]);\n\nEXTERNAL_SYMBOL ::=\nIDENTIFIER\n| static_string_EXPRESSION\n| \""\""\n\nPARAMETER_TYPES ::=\nnull\n| TYPE_DESIGNATOR {, TYPE_DESIGNATOR}\n\nTYPE_DESIGNATOR ::=\nsubtype_NAME\n| subtype_Name ' Access\n\nMECHANISM ::=\nMECHANISM_NAME\n| (MECHANISM_ASSOCIATION {, MECHANISM_ASSOCIATION})\n\nMECHANISM_ASSOCIATION ::=\n[formal_parameter_NAME =>] MECHANISM_NAME\n\nMECHANISM_NAME ::= Value | Reference\n\nThis pragma is identical to `Export_Function' except that it applies to\na procedure rather than a function and the parameters `Result_Type' and\n`Result_Mechanism' are not permitted.  GNAT does not require a separate\npragma `Export', but if none is present, `Convention Ada' is assumed,\nwhich is usually not what is wanted, so it is usually appropriate to\nuse this pragma in conjunction with a `Export' or `Convention' pragma\nthat specifies the desired foreign convention.\n\nSpecial treatment is given if the EXTERNAL is an explicit null string\nor a static string expressions that evaluates to the null string. In\nthis case, no external name is generated. This form still allows the\nspecification of parameter mechanisms.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Export_Procedure""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Export_Value (\n[Value     =>] static_integer_EXPRESSION,\n[Link_Name =>] static_string_EXPRESSION);\n\nThis pragma serves to export a static integer value for external use.\nThe first argument specifies the value to be exported. The Link_Name\nargument specifies the symbolic name to be associated with the integer\nvalue. This pragma is useful for defining a named static value in Ada\nthat can be referenced in assembly language units to be linked with the\napplication. This pragma is currently supported only for the AAMP\ntarget and is ignored for other targets.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Export_Value""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Export_Valued_Procedure (\n[Internal        =>] LOCAL_NAME\n[, [External        =>] EXTERNAL_SYMBOL]\n[, [Parameter_Types =>] PARAMETER_TYPES]\n[, [Mechanism       =>] MECHANISM]);\n\nEXTERNAL_SYMBOL ::=\nIDENTIFIER\n| static_string_EXPRESSION\n| \""\""\n\nPARAMETER_TYPES ::=\nnull\n| TYPE_DESIGNATOR {, TYPE_DESIGNATOR}\n\nTYPE_DESIGNATOR ::=\nsubtype_NAME\n| subtype_Name ' Access\n\nMECHANISM ::=\nMECHANISM_NAME\n| (MECHANISM_ASSOCIATION {, MECHANISM_ASSOCIATION})\n\nMECHANISM_ASSOCIATION ::=\n[formal_parameter_NAME =>] MECHANISM_NAME\n\nMECHANISM_NAME ::= Value | Reference\n\nThis pragma is identical to `Export_Procedure' except that the first\nparameter of `LOCAL_NAME', which must be present, must be of mode\n`out', and externally the subprogram is treated as a function with this\nparameter as the result of the function.  GNAT provides for this\ncapability to allow the use of `out' and `in out' parameters in\ninterfacing to external functions (which are not permitted in Ada\nfunctions).  GNAT does not require a separate pragma `Export', but if\nnone is present, `Convention Ada' is assumed, which is almost certainly\nnot what is wanted since the whole point of this pragma is to interface\nwith foreign language functions, so it is usually appropriate to use\nthis pragma in conjunction with a `Export' or `Convention' pragma that\nspecifies the desired foreign convention.\n\nSpecial treatment is given if the EXTERNAL is an explicit null string\nor a static string expressions that evaluates to the null string. In\nthis case, no external name is generated. This form still allows the\nspecification of parameter mechanisms.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Export_Valued_Procedure""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Extend_System ([Name =>] IDENTIFIER);\n\nThis pragma is used to provide backwards compatibility with other\nimplementations that extend the facilities of package `System'.  In\nGNAT, `System' contains only the definitions that are present in the\nAda RM.  However, other implementations, notably the DEC Ada 83\nimplementation, provide many extensions to package `System'.\n\nFor each such implementation accommodated by this pragma, GNAT provides\na package `Aux_`xxx'', e.g., `Aux_DEC' for the DEC Ada 83\nimplementation, which provides the required additional definitions.  You\ncan use this package in two ways.  You can `with' it in the normal way\nand access entities either by selection or using a `use' clause.  In\nthis case no special processing is required.\n\nHowever, if existing code contains references such as `System.`xxx''\nwhere `xxx' is an entity in the extended definitions provided in\npackage `System', you may use this pragma to extend visibility in\n`System' in a non-standard way that provides greater compatibility with\nthe existing code.  Pragma `Extend_System' is a configuration pragma\nwhose single argument is the name of the package containing the\nextended definition (e.g., `Aux_DEC' for the DEC Ada case).  A unit\ncompiled under control of this pragma will be processed using special\nvisibility processing that looks in package `System.Aux_`xxx'' where\n`Aux_`xxx'' is the pragma argument for any entity referenced in package\n`System', but not found in package `System'.\n\nYou can use this pragma either to access a predefined `System'\nextension supplied with the compiler, for example `Aux_DEC' or you can\nconstruct your own extension unit following the above definition.  Note\nthat such a package is a child of `System' and thus is considered part\nof the implementation.  To compile it you will have to use the `-gnatg'\nswitch for compiling System units, as explained in the GNAT User's\nGuide.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Extend_System""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Extensions_Allowed (On | Off);\n\nThis configuration pragma enables or disables the implementation\nextension mode (the use of Off as a parameter cancels the effect of the\n`-gnatX' command switch).\n\nIn extension mode, the latest version of the Ada language is\nimplemented (currently Ada 2012), and in addition a small number of\nGNAT specific extensions are recognized as follows:\n\n`Constrained attribute for generic objects'\nThe `Constrained' attribute is permitted for objects of generic\ntypes. The result indicates if the corresponding actual is\nconstrained.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Extensions_Allowed""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Extensions_Visible [ (boolean_EXPRESSION) ];\n\nFor the semantics of this pragma, see the entry for aspect\n`Extensions_Visible' in the SPARK 2014 Reference Manual, section 6.1.7.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Extensions_Visible""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma External (\n[   Convention    =>] convention_IDENTIFIER,\n[   Entity        =>] LOCAL_NAME\n[, [External_Name =>] static_string_EXPRESSION ]\n[, [Link_Name     =>] static_string_EXPRESSION ]);\n\nThis pragma is identical in syntax and semantics to pragma `Export' as\ndefined in the Ada Reference Manual.  It is provided for compatibility\nwith some Ada 83 compilers that used this pragma for exactly the same\npurposes as pragma `Export' before the latter was standardized.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""External""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma External_Name_Casing (\nUppercase | Lowercase\n[, Uppercase | Lowercase | As_Is]);\n\nThis pragma provides control over the casing of external names\nassociated with Import and Export pragmas.  There are two cases to\nconsider:\n\n* Implicit external names\n\nImplicit external names are derived from identifiers.  The most\ncommon case arises when a standard Ada Import or Export pragma is\nused with only two arguments, as in:\n\npragma Import (C, C_Routine);\n\nSince Ada is a case-insensitive language, the spelling of the\nidentifier in the Ada source program does not provide any\ninformation on the desired casing of the external name, and so a\nconvention is needed.  In GNAT the default treatment is that such\nnames are converted to all lower case letters.  This corresponds\nto the normal C style in many environments.  The first argument of\npragma `External_Name_Casing' can be used to control this\ntreatment.  If `Uppercase' is specified, then the name will be\nforced to all uppercase letters.  If `Lowercase' is specified,\nthen the normal default of all lower case letters will be used.\n\nThis same implicit treatment is also used in the case of extended\nDEC Ada 83 compatible Import and Export pragmas where an external\nname is explicitly specified using an identifier rather than a\nstring.\n\n* Explicit external names\n\nExplicit external names are given as string literals.  The most\ncommon case arises when a standard Ada Import or Export pragma is\nused with three arguments, as in:\n\npragma Import (C, C_Routine, \""C_routine\"");\n\nIn this case, the string literal normally provides the exact\ncasing required for the external name.  The second argument of\npragma `External_Name_Casing' may be used to modify this behavior.\nIf `Uppercase' is specified, then the name will be forced to all\nuppercase letters.  If `Lowercase' is specified, then the name\nwill be forced to all lowercase letters.  A specification of\n`As_Is' provides the normal default behavior in which the casing is\ntaken from the string provided.\n\nThis pragma may appear anywhere that a pragma is valid. In particular,\nit can be used as a configuration pragma in the `gnat.adc' file, in\nwhich case it applies to all subsequent compilations, or it can be used\nas a program unit pragma, in which case it only applies to the current\nunit, or it can be used more locally to control individual\nImport/Export pragmas.\n\nIt was primarily intended for use with OpenVMS systems, where many\ncompilers convert all symbols to upper case by default.  For\ninterfacing to such compilers (e.g., the DEC C compiler), it may be\nconvenient to use the pragma:\n\npragma External_Name_Casing (Uppercase, Uppercase);\n\nto enforce the upper casing of all external symbols.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""External_Name_Casing""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Fast_Math;\n\nThis is a configuration pragma which activates a mode in which speed is\nconsidered more important for floating-point operations than absolutely\naccurate adherence to the requirements of the standard. Currently the\nfollowing operations are affected:\n\n`Complex Multiplication'\nThe normal simple formula for complex multiplication can result in\nintermediate overflows for numbers near the end of the range. The\nAda standard requires that this situation be detected and\ncorrected by scaling, but in Fast_Math mode such cases will simply\nresult in overflow. Note that to take advantage of this you must\ninstantiate your own version of\n`Ada.Numerics.Generic_Complex_Types' under control of the pragma,\nrather than use the preinstantiated versions.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Fast_Math""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Favor_Top_Level (type_NAME);\n\nThe argument of pragma `Favor_Top_Level' must be a named\naccess-to-subprogram type. This pragma is an efficiency hint to the\ncompiler, regarding the use of `'Access' or `'Unrestricted_Access' on\nnested (non-library-level) subprograms.  The pragma means that nested\nsubprograms are not used with this type, or are rare, so that the\ngenerated code should be efficient in the top-level case.  When this\npragma is used, dynamically generated trampolines may be used on some\ntargets for nested subprograms. See restriction\n`No_Implicit_Dynamic_Code'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Favor_Top_Level""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Finalize_Storage_Only (first_subtype_LOCAL_NAME);\n\nThe argument of pragma `Finalize_Storage_Only' must denote a local type\nwhich is derived from `Ada.Finalization.Controlled' or\n`Limited_Controlled'. The pragma suppresses the call to `Finalize' for\ndeclared library-level objects of the argument type. This is mostly\nuseful for types where finalization is only used to deal with storage\nreclamation since in most environments it is not necessary to reclaim\nmemory just before terminating execution, hence the name. Note that\nthis pragma does not suppress Finalize calls for library-level\nheap-allocated objects (see pragma `No_Heap_Finalization').""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Finalize_Storage_Only""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Float_Representation (FLOAT_REP[, float_type_LOCAL_NAME]);\n\nFLOAT_REP ::= VAX_Float | IEEE_Float\n\nIn the one argument form, this pragma is a configuration pragma which\nallows control over the internal representation chosen for the\npredefined floating point types declared in the packages `Standard' and\n`System'. This pragma is only provided for compatibility and has no\neffect.\n\nThe two argument form specifies the representation to be used for the\nspecified floating-point type. The argument must be `IEEE_Float' to\nspecify the use of IEEE format, as follows:\n\n* For a digits value of 6, 32-bit IEEE short format will be used.\n\n* For a digits value of 15, 64-bit IEEE long format will be used.\n\n* No other value of digits is permitted.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Float_Representation""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Ghost [ (boolean_EXPRESSION) ];\n\nFor the semantics of this pragma, see the entry for aspect `Ghost' in\nthe SPARK 2014 Reference Manual, section 6.9.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Ghost""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Global (GLOBAL_SPECIFICATION);\n\nGLOBAL_SPECIFICATION ::=\nnull\n| (GLOBAL_LIST)\n| (MODED_GLOBAL_LIST {, MODED_GLOBAL_LIST})\n\nMODED_GLOBAL_LIST ::= MODE_SELECTOR => GLOBAL_LIST\n\nMODE_SELECTOR ::= In_Out | Input | Output | Proof_In\nGLOBAL_LIST   ::= GLOBAL_ITEM | (GLOBAL_ITEM {, GLOBAL_ITEM})\nGLOBAL_ITEM   ::= NAME\n\nFor the semantics of this pragma, see the entry for aspect `Global' in\nthe SPARK 2014 Reference Manual, section 6.1.4.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Global""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Ident (static_string_EXPRESSION);\n\nThis pragma is identical in effect to pragma `Comment'. It is provided\nfor compatibility with other Ada compilers providing this pragma.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Ident""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Ignore_Pragma (pragma_IDENTIFIER);\n\nThis is a configuration pragma that takes a single argument that is a\nsimple identifier. Any subsequent use of a pragma whose pragma\nidentifier matches this argument will be silently ignored. This may be\nuseful when legacy code or code intended for compilation with some\nother compiler contains pragmas that match the name, but not the exact\nimplementation, of a GNAT pragma. The use of this pragma allows such\npragmas to be ignored, which may be useful in CodePeer mode, or during\nporting of legacy code.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Ignore_Pragma""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Implementation_Defined (local_NAME);\n\nThis pragma marks a previously declared entity as\nimplementation-defined.  For an overloaded entity, applies to the most\nrecent homonym.\n\npragma Implementation_Defined;\n\nThe form with no arguments appears anywhere within a scope, most\ntypically a package spec, and indicates that all entities that are\ndefined within the package spec are Implementation_Defined.\n\nThis pragma is used within the GNAT runtime library to identify\nimplementation-defined entities introduced in language-defined units,\nfor the purpose of implementing the No_Implementation_Identifiers\nrestriction.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Implementation_Defined""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Implemented (procedure_LOCAL_NAME, implementation_kind);\n\nimplementation_kind ::= By_Entry | By_Protected_Procedure | By_Any\n\nThis is an Ada 2012 representation pragma which applies to protected,\ntask and synchronized interface primitives. The use of pragma\nImplemented provides a way to impose a static requirement on the\noverriding operation by adhering to one of the three implementation\nkinds: entry, protected procedure or any of the above. This pragma is\navailable in all earlier versions of Ada as an implementation-defined\npragma.\n\ntype Synch_Iface is synchronized interface;\nprocedure Prim_Op (Obj : in out Iface) is abstract;\npragma Implemented (Prim_Op, By_Protected_Procedure);\n\nprotected type Prot_1 is new Synch_Iface with\nprocedure Prim_Op;  --  Legal\nend Prot_1;\n\nprotected type Prot_2 is new Synch_Iface with\nentry Prim_Op;      --  Illegal\nend Prot_2;\n\ntask type Task_Typ is new Synch_Iface with\nentry Prim_Op;      --  Illegal\nend Task_Typ;\n\nWhen applied to the procedure_or_entry_NAME of a requeue statement,\npragma Implemented determines the runtime behavior of the requeue.\nImplementation kind By_Entry guarantees that the action of requeueing\nwill proceed from an entry to another entry. Implementation kind\nBy_Protected_Procedure transforms the requeue into a dispatching call,\nthus eliminating the chance of blocking. Kind By_Any shares the\nbehavior of By_Entry and By_Protected_Procedure depending on the\ntarget's overriding subprogram kind.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Implemented""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Implicit_Packing;\n\nThis is a configuration pragma that requests implicit packing for packed\narrays for which a size clause is given but no explicit pragma Pack or\nspecification of Component_Size is present. It also applies to records\nwhere no record representation clause is present. Consider this example:\n\ntype R is array (0 .. 7) of Boolean;\nfor R'Size use 8;\n\nIn accordance with the recommendation in the RM (RM 13.3(53)), a Size\nclause does not change the layout of a composite object. So the Size\nclause in the above example is normally rejected, since the default\nlayout of the array uses 8-bit components, and thus the array requires\na minimum of 64 bits.\n\nIf this declaration is compiled in a region of code covered by an\noccurrence of the configuration pragma Implicit_Packing, then the Size\nclause in this and similar examples will cause implicit packing and\nthus be accepted. For this implicit packing to occur, the type in\nquestion must be an array of small components whose size is known at\ncompile time, and the Size clause must specify the exact size that\ncorresponds to the number of elements in the array multiplied by the\nsize in bits of the component type (both single and multi-dimensioned\narrays can be controlled with this pragma).\n\nSimilarly, the following example shows the use in the record case\n\ntype r is record\na, b, c, d, e, f, g, h : boolean;\nchr                    : character;\nend record;\nfor r'size use 16;\n\nWithout a pragma Pack, each Boolean field requires 8 bits, so the\nminimum size is 72 bits, but with a pragma Pack, 16 bits would be\nsufficient. The use of pragma Implicit_Packing allows this record\ndeclaration to compile without an explicit pragma Pack.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Implicit_Packing""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Import ([Convention=>]convention_identifier,[Entity=>]local_name;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Import""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Import_Function (\n[Internal                 =>] LOCAL_NAME,\n[, [External                 =>] EXTERNAL_SYMBOL]\n[, [Parameter_Types          =>] PARAMETER_TYPES]\n[, [Result_Type              =>] SUBTYPE_MARK]\n[, [Mechanism                =>] MECHANISM]\n[, [Result_Mechanism         =>] MECHANISM_NAME]);\n\nEXTERNAL_SYMBOL ::=\nIDENTIFIER\n| static_string_EXPRESSION\n\nPARAMETER_TYPES ::=\nnull\n| TYPE_DESIGNATOR {, TYPE_DESIGNATOR}\n\nTYPE_DESIGNATOR ::=\nsubtype_NAME\n| subtype_Name ' Access\n\nMECHANISM ::=\nMECHANISM_NAME\n| (MECHANISM_ASSOCIATION {, MECHANISM_ASSOCIATION})\n\nMECHANISM_ASSOCIATION ::=\n[formal_parameter_NAME =>] MECHANISM_NAME\n\nMECHANISM_NAME ::=\nValue\n| Reference\n\nThis pragma is used in conjunction with a pragma `Import' to specify\nadditional information for an imported function.  The pragma `Import'\n(or equivalent pragma `Interface') must precede the `Import_Function'\npragma and both must appear in the same declarative part as the\nfunction specification.\n\nThe `Internal' argument must uniquely designate the function to which\nthe pragma applies.  If more than one function name exists of this name\nin the declarative part you must use the `Parameter_Types' and\n`Result_Type' parameters to achieve the required unique designation.\nSubtype marks in these parameters must exactly match the subtypes in\nthe corresponding function specification, using positional notation to\nmatch parameters with subtype marks.  The form with an `'Access'\nattribute can be used to match an anonymous access parameter.\n\nYou may optionally use the `Mechanism' and `Result_Mechanism'\nparameters to specify passing mechanisms for the parameters and result.\nIf you specify a single mechanism name, it applies to all parameters.\nOtherwise you may specify a mechanism on a parameter by parameter basis\nusing either positional or named notation.  If the mechanism is not\nspecified, the default mechanism is used.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Import_Function""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Import_Object\n[Internal =>] LOCAL_NAME\n[, [External =>] EXTERNAL_SYMBOL]\n[, [Size     =>] EXTERNAL_SYMBOL]);\n\nEXTERNAL_SYMBOL ::=\nIDENTIFIER\n| static_string_EXPRESSION\n\nThis pragma designates an object as imported, and apart from the\nextended rules for external symbols, is identical in effect to the use\nof the normal `Import' pragma applied to an object.  Unlike the\nsubprogram case, you need not use a separate `Import' pragma, although\nyou may do so (and probably should do so from a portability point of\nview).  `size' is syntax checked, but otherwise ignored by GNAT.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Import_Object""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Import_Procedure (\n[Internal                 =>] LOCAL_NAME\n[, [External                 =>] EXTERNAL_SYMBOL]\n[, [Parameter_Types          =>] PARAMETER_TYPES]\n[, [Mechanism                =>] MECHANISM]);\n\nEXTERNAL_SYMBOL ::=\nIDENTIFIER\n| static_string_EXPRESSION\n\nPARAMETER_TYPES ::=\nnull\n| TYPE_DESIGNATOR {, TYPE_DESIGNATOR}\n\nTYPE_DESIGNATOR ::=\nsubtype_NAME\n| subtype_Name ' Access\n\nMECHANISM ::=\nMECHANISM_NAME\n| (MECHANISM_ASSOCIATION {, MECHANISM_ASSOCIATION})\n\nMECHANISM_ASSOCIATION ::=\n[formal_parameter_NAME =>] MECHANISM_NAME\n\nMECHANISM_NAME ::= Value | Reference\n\nThis pragma is identical to `Import_Function' except that it applies to\na procedure rather than a function and the parameters `Result_Type' and\n`Result_Mechanism' are not permitted.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Import_Procedure""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Import_Valued_Procedure (\n[Internal                 =>] LOCAL_NAME\n[, [External                 =>] EXTERNAL_SYMBOL]\n[, [Parameter_Types          =>] PARAMETER_TYPES]\n[, [Mechanism                =>] MECHANISM]);\n\nEXTERNAL_SYMBOL ::=\nIDENTIFIER\n| static_string_EXPRESSION\n\nPARAMETER_TYPES ::=\nnull\n| TYPE_DESIGNATOR {, TYPE_DESIGNATOR}\n\nTYPE_DESIGNATOR ::=\nsubtype_NAME\n| subtype_Name ' Access\n\nMECHANISM ::=\nMECHANISM_NAME\n| (MECHANISM_ASSOCIATION {, MECHANISM_ASSOCIATION})\n\nMECHANISM_ASSOCIATION ::=\n[formal_parameter_NAME =>] MECHANISM_NAME\n\nMECHANISM_NAME ::= Value | Reference\n\nThis pragma is identical to `Import_Procedure' except that the first\nparameter of `LOCAL_NAME', which must be present, must be of mode\n`out', and externally the subprogram is treated as a function with this\nparameter as the result of the function.  The purpose of this\ncapability is to allow the use of `out' and `in out' parameters in\ninterfacing to external functions (which are not permitted in Ada\nfunctions).  You may optionally use the `Mechanism' parameters to\nspecify passing mechanisms for the parameters.  If you specify a single\nmechanism name, it applies to all parameters.  Otherwise you may\nspecify a mechanism on a parameter by parameter basis using either\npositional or named notation.  If the mechanism is not specified, the\ndefault mechanism is used.\n\nNote that it is important to use this pragma in conjunction with a\nseparate pragma Import that specifies the desired convention, since\notherwise the default convention is Ada, which is almost certainly not\nwhat is required.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Import_Valued_Procedure""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Independent (Local_NAME);\n\nThis pragma is standard in Ada 2012 mode (which also provides an aspect\nof the same name). It is also available as an implementation-defined\npragma in all earlier versions. It specifies that the designated object\nor all objects of the designated type must be independently\naddressable. This means that separate tasks can safely manipulate such\nobjects. For example, if two components of a record are independent,\nthen two separate tasks may access these two components.  This may place\nconstraints on the representation of the object (for instance\nprohibiting tight packing).""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Independent""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Independent_Components (Local_NAME);\n\nThis pragma is standard in Ada 2012 mode (which also provides an aspect\nof the same name). It is also available as an implementation-defined\npragma in all earlier versions. It specifies that the components of the\ndesignated object, or the components of each object of the designated\ntype, must be independently addressable. This means that separate tasks\ncan safely manipulate separate components in the composite object. This\nmay place constraints on the representation of the object (for instance\nprohibiting tight packing).""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Independent_Components""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Initial_Condition (boolean_EXPRESSION);\n\nFor the semantics of this pragma, see the entry for aspect\n`Initial_Condition' in the SPARK 2014 Reference Manual, section 7.1.6.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Initial_Condition""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Initialize_Scalars\n[ ( TYPE_VALUE_PAIR {, TYPE_VALUE_PAIR} ) ];\n\nTYPE_VALUE_PAIR ::=\nSCALAR_TYPE => static_EXPRESSION\n\nSCALAR_TYPE :=\nShort_Float\n| Float\n| Long_Float\n| Long_Long_Flat\n| Signed_8\n| Signed_16\n| Signed_32\n| Signed_64\n| Unsigned_8\n| Unsigned_16\n| Unsigned_32\n| Unsigned_64\n\nThis pragma is similar to `Normalize_Scalars' conceptually but has two\nimportant differences.\n\nFirst, there is no requirement for the pragma to be used uniformly in\nall units of a partition. In particular, it is fine to use this just\nfor some or all of the application units of a partition, without\nneeding to recompile the run-time library. In the case where some units\nare compiled with the pragma, and some without, then a declaration of a\nvariable where the type is defined in package Standard or is locally\ndeclared will always be subject to initialization, as will any\ndeclaration of a scalar variable. For composite variables, whether the\nvariable is initialized may also depend on whether the package in which\nthe type of the variable is declared is compiled with the pragma.\n\nThe other important difference is that the programmer can control the\nvalue used for initializing scalar objects. This effect can be achieved\nin several different ways:\n\n* At compile time, the programmer can specify the invalid value for a\nparticular family of scalar types using the optional arguments of\nthe pragma.\n\nThe compile-time approach is intended to optimize the generated\ncode for the pragma, by possibly using fast operations such as\n`memset'.\n\n* At bind time, the programmer has several options:\n\n* Initialization with invalid values (similar to\nNormalize_Scalars, though for Initialize_Scalars it is not\nalways possible to determine the invalid values in complex\ncases like signed component fields with nonstandard sizes).\n\n* Initialization with high values.\n\n* Initialization with low values.\n\n* Initialization with a specific bit pattern.\n\nSee the GNAT User's Guide for binder options for specifying these\ncases.\n\nThe bind-time approach is intended to provide fast turnaround for\ntesting with different values, without having to recompile the\nprogram.\n\n* At execution time, the programmer can speify the invalid values\nusing an environment variable. See the GNAT User's Guide for\ndetails.\n\nThe execution-time approach is intended to provide fast turnaround\nfor testing with different values, without having to recompile and\nrebind the program.\n\nNote that pragma `Initialize_Scalars' is particularly useful in\nconjunction with the enhanced validity checking that is now provided in\nGNAT, which checks for invalid values under more conditions. Using this\nfeature (see description of the `-gnatV' flag in the GNAT User's Guide)\nin conjunction with pragma `Initialize_Scalars' provides a powerful new\ntool to assist in the detection of problems caused by uninitialized\nvariables.\n\nNote: the use of `Initialize_Scalars' has a fairly extensive effect on\nthe generated code. This may cause your code to be substantially\nlarger. It may also cause an increase in the amount of stack required,\nso it is probably a good idea to turn on stack checking (see\ndescription of stack checking in the GNAT User's Guide) when using this\npragma.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Initialize_Scalars""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Initializes (INITIALIZATION_LIST);\n\nINITIALIZATION_LIST ::=\nnull\n| (INITIALIZATION_ITEM {, INITIALIZATION_ITEM})\n\nINITIALIZATION_ITEM ::= name [=> INPUT_LIST]\n\nINPUT_LIST ::=\nnull\n|  INPUT\n| (INPUT {, INPUT})\n\nINPUT ::= name\n\nFor the semantics of this pragma, see the entry for aspect\n`Initializes' in the SPARK 2014 Reference Manual, section 7.1.5.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Initializes""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Inline;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Inline""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Inline_Always (NAME [, NAME]);\n\nSimilar to pragma `Inline' except that inlining is unconditional.\nInline_Always instructs the compiler to inline every direct call to the\nsubprogram or else to emit a compilation error, independently of any\noption, in particular `-gnatn' or `-gnatN' or the optimization level.\nIt is an error to take the address or access of `NAME'. It is also an\nerror to apply this pragma to a primitive operation of a tagged type.\nThanks to such restrictions, the compiler is allowed to remove the\nout-of-line body of `NAME'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Inline_Always""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Inline_Generic (GNAME {, GNAME});\n\nGNAME ::= generic_unit_NAME | generic_instance_NAME\n\nThis pragma is provided for compatibility with Dec Ada 83. It has no\neffect in GNAT (which always inlines generics), other than to check\nthat the given names are all names of generic units or generic\ninstances.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Inline_Generic""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Inspection_Point [(object_name {, object_name})];""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Inspection_Point""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Interface (\n[Convention    =>] convention_identifier,\n[Entity        =>] local_NAME\n[, [External_Name =>] static_string_expression]\n[, [Link_Name     =>] static_string_expression]);\n\nThis pragma is identical in syntax and semantics to the standard Ada\npragma `Import'.  It is provided for compatibility with Ada 83.  The\ndefinition is upwards compatible both with pragma `Interface' as\ndefined in the Ada 83 Reference Manual, and also with some extended\nimplementations of this pragma in certain Ada 83 implementations.  The\nonly difference between pragma `Interface' and pragma `Import' is that\nthere is special circuitry to allow both pragmas to appear for the same\nsubprogram entity (normally it is illegal to have multiple `Import'\npragmas. This is useful in maintaining Ada 83/Ada 95 compatibility and\nis compatible with other Ada 83 compilers.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Interface""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Interface_Name (\n[Entity        =>] LOCAL_NAME\n[, [External_Name =>] static_string_EXPRESSION]\n[, [Link_Name     =>] static_string_EXPRESSION]);\n\nThis pragma provides an alternative way of specifying the interface name\nfor an interfaced subprogram, and is provided for compatibility with Ada\n83 compilers that use the pragma for this purpose.  You must provide at\nleast one of `External_Name' or `Link_Name'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Interface_Name""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Interrupt_Handler (procedure_LOCAL_NAME);\n\nThis program unit pragma is supported for parameterless protected\nprocedures as described in Annex C of the Ada Reference Manual. On the\nAAMP target the pragma can also be specified for nonprotected\nparameterless procedures that are declared at the library level (which\nincludes procedures declared at the top level of a library package). In\nthe case of AAMP, when this pragma is applied to a nonprotected\nprocedure, the instruction `IERET' is generated for returns from the\nprocedure, enabling maskable interrupts, in place of the normal return\ninstruction.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Interrupt_Handler""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Interrupt_Priority;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Interrupt_Priority""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Interrupt_State\n([Name  =>] value,\n[State =>] SYSTEM | RUNTIME | USER);\n\nNormally certain interrupts are reserved to the implementation.  Any\nattempt to attach an interrupt causes Program_Error to be raised, as\ndescribed in RM C.3.2(22).  A typical example is the `SIGINT' interrupt\nused in many systems for an `Ctrl-C' interrupt.  Normally this\ninterrupt is reserved to the implementation, so that `Ctrl-C' can be\nused to interrupt execution.  Additionally, signals such as `SIGSEGV',\n`SIGABRT', `SIGFPE' and `SIGILL' are often mapped to specific Ada\nexceptions, or used to implement run-time functions such as the `abort'\nstatement and stack overflow checking.\n\nPragma `Interrupt_State' provides a general mechanism for overriding\nsuch uses of interrupts.  It subsumes the functionality of pragma\n`Unreserve_All_Interrupts'.  Pragma `Interrupt_State' is not available\non Windows.  On all other platforms than VxWorks, it applies to\nsignals; on VxWorks, it applies to vectored hardware interrupts and may\nbe used to mark interrupts required by the board support package as\nreserved.\n\nInterrupts can be in one of three states:\n\n* System\n\nThe interrupt is reserved (no Ada handler can be installed), and\nthe Ada run-time may not install a handler. As a result you are\nguaranteed standard system default action if this interrupt is\nraised. This also allows installing a low level handler via C APIs\nsuch as sigaction(), outside of Ada control.\n\n* Runtime\n\nThe interrupt is reserved (no Ada handler can be installed). The\nrun time is allowed to install a handler for internal control\npurposes, but is not required to do so.\n\n* User\n\nThe interrupt is unreserved.  The user may install an Ada handler\nvia Ada.Interrupts and pragma Interrupt_Handler or Attach_Handler\nto provide some other action.\n\nThese states are the allowed values of the `State' parameter of the\npragma.  The `Name' parameter is a value of the type\n`Ada.Interrupts.Interrupt_ID'.  Typically, it is a name declared in\n`Ada.Interrupts.Names'.\n\nThis is a configuration pragma, and the binder will check that there\nare no inconsistencies between different units in a partition in how a\ngiven interrupt is specified. It may appear anywhere a pragma is legal.\n\nThe effect is to move the interrupt to the specified state.\n\nBy declaring interrupts to be SYSTEM, you guarantee the standard system\naction, such as a core dump.\n\nBy declaring interrupts to be USER, you guarantee that you can install\na handler.\n\nNote that certain signals on many operating systems cannot be caught and\nhandled by applications.  In such cases, the pragma is ignored.  See the\noperating system documentation, or the value of the array `Reserved'\ndeclared in the spec of package `System.OS_Interface'.\n\nOverriding the default state of signals used by the Ada runtime may\ninterfere with an application's runtime behavior in the cases of the\nsynchronous signals, and in the case of the signal used to implement\nthe `abort' statement.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Interrupt_State""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Invariant\n([Entity =>]    private_type_LOCAL_NAME,\n[Check  =>]    EXPRESSION\n[,[Message =>] String_Expression]);\n\nThis pragma provides exactly the same capabilities as the\nType_Invariant aspect defined in AI05-0146-1, and in the Ada 2012\nReference Manual. The Type_Invariant aspect is fully implemented in Ada\n2012 mode, but since it requires the use of the aspect syntax, which is\nnot available except in 2012 mode, it is not possible to use the\nType_Invariant aspect in earlier versions of Ada. However the Invariant\npragma may be used in any version of Ada. Also note that the aspect\nInvariant is a synonym in GNAT for the aspect Type_Invariant, but there\nis no pragma Type_Invariant.\n\nThe pragma must appear within the visible part of the package\nspecification, after the type to which its Entity argument appears. As\nwith the Invariant aspect, the Check expression is not analyzed until\nthe end of the visible part of the package, so it may contain forward\nreferences. The Message argument, if present, provides the exception\nmessage used if the invariant is violated. If no Message parameter is\nprovided, a default message that identifies the line on which the\npragma appears is used.\n\nIt is permissible to have multiple Invariants for the same type entity,\nin which case they are and'ed together. It is permissible to use this\npragma in Ada 2012 mode, but you cannot have both an invariant aspect\nand an invariant pragma for the same entity.\n\nFor further details on the use of this pragma, see the Ada 2012\ndocumentation of the Type_Invariant aspect.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Invariant""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Keep_Names ([On =>] enumeration_first_subtype_LOCAL_NAME);\n\nThe `LOCAL_NAME' argument must refer to an enumeration first subtype in\nthe current declarative part. The effect is to retain the enumeration\nliteral names for use by `Image' and `Value' even if a global\n`Discard_Names' pragma applies. This is useful when you want to\ngenerally suppress enumeration literal names and for example you\ntherefore use a `Discard_Names' pragma in the `gnat.adc' file, but you\nwant to retain the names for specific enumeration types.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Keep_Names""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma License (Unrestricted | GPL | Modified_GPL | Restricted);\n\nThis pragma is provided to allow automated checking for appropriate\nlicense conditions with respect to the standard and modified GPL.  A\npragma `License', which is a configuration pragma that typically\nappears at the start of a source file or in a separate `gnat.adc' file,\nspecifies the licensing conditions of a unit as follows:\n\n* Unrestricted This is used for a unit that can be freely used with\nno license restrictions.  Examples of such units are public domain\nunits, and units from the Ada Reference Manual.\n\n* GPL This is used for a unit that is licensed under the unmodified\nGPL, and which therefore cannot be `with'ed by a restricted unit.\n\n* Modified_GPL This is used for a unit licensed under the GNAT\nmodified GPL that includes a special exception paragraph that\nspecifically permits the inclusion of the unit in programs without\nrequiring the entire program to be released under the GPL.\n\n* Restricted This is used for a unit that is restricted in that it\nis not permitted to depend on units that are licensed under the\nGPL.  Typical examples are proprietary code that is to be released\nunder more restrictive license conditions.  Note that restricted\nunits are permitted to `with' units which are licensed under the\nmodified GPL (this is the whole point of the modified GPL).\n\nNormally a unit with no `License' pragma is considered to have an\nunknown license, and no checking is done.  However, standard GNAT\nheaders are recognized, and license information is derived from them as\nfollows.\n\nA GNAT license header starts with a line containing 78 hyphens.  The\nfollowing comment text is searched for the appearance of any of the\nfollowing strings.\n\nIf the string 'GNU General Public License' is found, then the unit is\nassumed to have GPL license, unless the string 'As a special exception'\nfollows, in which case the license is assumed to be modified GPL.\n\nIf one of the strings 'This specification is adapted from the Ada\nSemantic Interface' or 'This specification is derived from the Ada\nReference Manual' is found then the unit is assumed to be unrestricted.\n\nThese default actions means that a program with a restricted license\npragma will automatically get warnings if a GPL unit is inappropriately\n`with'ed.  For example, the program:\n\nwith Sem_Ch3;\nwith GNAT.Sockets;\nprocedure Secret_Stuff is\n\nend Secret_Stuff\n\nif compiled with pragma `License' (`Restricted') in a `gnat.adc' file\nwill generate the warning:\n\nwith Sem_Ch3;\n|\n>>> license of withed unit \""Sem_Ch3\"" is incompatible\n\nwith GNAT.Sockets;\nprocedure Secret_Stuff is\n\nHere we get a warning on `Sem_Ch3' since it is part of the GNAT\ncompiler and is licensed under the GPL, but no warning for\n`GNAT.Sockets' which is part of the GNAT run time, and is therefore\nlicensed under the modified GPL.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""License""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Link_With (static_string_EXPRESSION {,static_string_EXPRESSION});\n\nThis pragma is provided for compatibility with certain Ada 83 compilers.\nIt has exactly the same effect as pragma `Linker_Options' except that\nspaces occurring within one of the string expressions are treated as\nseparators. For example, in the following case:\n\npragma Link_With (\""-labc -ldef\"");\n\nresults in passing the strings `-labc' and `-ldef' as two separate\narguments to the linker. In addition pragma Link_With allows multiple\narguments, with the same effect as successive pragmas.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Link_With""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Linker_Alias (\n[Entity =>] LOCAL_NAME,\n[Target =>] static_string_EXPRESSION);\n\n`LOCAL_NAME' must refer to an object that is declared at the library\nlevel. This pragma establishes the given entity as a linker alias for\nthe given target. It is equivalent to `__attribute__((alias))' in GNU C\nand causes `LOCAL_NAME' to be emitted as an alias for the symbol\n`static_string_EXPRESSION' in the object file, that is to say no space\nis reserved for `LOCAL_NAME' by the assembler and it will be resolved\nto the same address as `static_string_EXPRESSION' by the linker.\n\nThe actual linker name for the target must be used (e.g., the fully\nencoded name with qualification in Ada, or the mangled name in C++), or\nit must be declared using the C convention with `pragma Import' or\n`pragma Export'.\n\nNot all target machines support this pragma. On some of them it is\naccepted only if `pragma Weak_External' has been applied to\n`LOCAL_NAME'.\n\n--  Example of the use of pragma Linker_Alias\n\npackage p is\ni : Integer := 1;\npragma Export (C, i);\n\nnew_name_for_i : Integer;\npragma Linker_Alias (new_name_for_i, \""i\"");\nend p;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Linker_Alias""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Linker_Constructor (procedure_LOCAL_NAME);\n\n`procedure_LOCAL_NAME' must refer to a parameterless procedure that is\ndeclared at the library level. A procedure to which this pragma is\napplied will be treated as an initialization routine by the linker.  It\nis equivalent to `__attribute__((constructor))' in GNU C and causes\n`procedure_LOCAL_NAME' to be invoked before the entry point of the\nexecutable is called (or immediately after the shared library is loaded\nif the procedure is linked in a shared library), in particular before\nthe Ada run-time environment is set up.\n\nBecause of these specific contexts, the set of operations such a\nprocedure can perform is very limited and the type of objects it can\nmanipulate is essentially restricted to the elementary types. In\nparticular, it must only contain code to which pragma Restrictions\n(No_Elaboration_Code) applies.\n\nThis pragma is used by GNAT to implement auto-initialization of shared\nStand Alone Libraries, which provides a related capability without the\nrestrictions listed above. Where possible, the use of Stand Alone\nLibraries is preferable to the use of this pragma.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Linker_Constructor""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Linker_Destructor (procedure_LOCAL_NAME);\n\n`procedure_LOCAL_NAME' must refer to a parameterless procedure that is\ndeclared at the library level. A procedure to which this pragma is\napplied will be treated as a finalization routine by the linker.  It is\nequivalent to `__attribute__((destructor))' in GNU C and causes\n`procedure_LOCAL_NAME' to be invoked after the entry point of the\nexecutable has exited (or immediately before the shared library is\nunloaded if the procedure is linked in a shared library), in particular\nafter the Ada run-time environment is shut down.\n\nSee `pragma Linker_Constructor' for the set of restrictions that apply\nbecause of these specific contexts.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Linker_Destructor""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Linker_Options (string_expression);""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Linker_Options""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Linker_Section (\n[Entity  =>] LOCAL_NAME,\n[Section =>] static_string_EXPRESSION);\n\n`LOCAL_NAME' must refer to an object, type, or subprogram that is\ndeclared at the library level. This pragma specifies the name of the\nlinker section for the given entity. It is equivalent to\n`__attribute__((section))' in GNU C and causes `LOCAL_NAME' to be\nplaced in the `static_string_EXPRESSION' section of the executable\n(assuming the linker doesn't rename the section).  GNAT also provides\nan implementation defined aspect of the same name.\n\nIn the case of specifying this aspect for a type, the effect is to\nspecify the corresponding section for all library-level objects of the\ntype that do not have an explicit linker section set. Note that this\nonly applies to whole objects, not to components of composite objects.\n\nIn the case of a subprogram, the linker section applies to all\npreviously declared matching overloaded subprograms in the current\ndeclarative part which do not already have a linker section assigned.\nThe linker section aspect is useful in this case for specifying\ndifferent linker sections for different elements of such an overloaded\nset.\n\nNote that an empty string specifies that no linker section is specified.\nThis is not quite the same as omitting the pragma or aspect, since it\ncan be used to specify that one element of an overloaded set of\nsubprograms has the default linker section, or that one object of a\ntype for which a linker section is specified should has the default\nlinker section.\n\nThe compiler normally places library-level entities in standard sections\ndepending on the class: procedures and functions generally go in the\n`.text' section, initialized variables in the `.data' section and\nuninitialized variables in the `.bss' section.\n\nOther, special sections may exist on given target machines to map\nspecial hardware, for example I/O ports or flash memory. This pragma is\na means to defer the final layout of the executable to the linker, thus\nfully working at the symbolic level with the compiler.\n\nSome file formats do not support arbitrary sections so not all target\nmachines support this pragma. The use of this pragma may cause a program\nexecution to be erroneous if it is used to place an entity into an\ninappropriate section (e.g., a modified variable into the `.text'\nsection). See also `pragma Persistent_BSS'.\n\n--  Example of the use of pragma Linker_Section\n\npackage IO_Card is\nPort_A : Integer;\npragma Volatile (Port_A);\npragma Linker_Section (Port_A, \"".bss.port_a\"");\n\nPort_B : Integer;\npragma Volatile (Port_B);\npragma Linker_Section (Port_B, \"".bss.port_b\"");\n\ntype Port_Type is new Integer with Linker_Section => \"".bss\"";\nPA : Port_Type with Linker_Section => \"".bss.PA\"";\nPB : Port_Type; --  ends up in linker section \"".bss\""\n\nprocedure Q with Linker_Section => \""Qsection\"";\nend IO_Card;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Linker_Section""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma List (identifier);""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""List""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax: This pragma may be specified for protected types or objects. It\nspecifies that the implementation of protected operations must be\nimplemented without locks.  Compilation fails if the compiler cannot\ngenerate lock-free code for the operations.\n\nThe current conditions required to support this pragma are:\n\n* Protected type declarations may not contain entries\n\n* Protected subprogram declarations may not have nonelementary\nparameters\n\nIn addition, each protected subprogram body must satisfy:\n\n* May reference only one protected component\n\n* May not reference nonconstant entities outside the protected\nsubprogram scope.\n\n* May not contain address representation items, allocators, or\nquantified expressions.\n\n* May not contain delay, goto, loop, or procedure-call statements.\n\n* May not contain exported and imported entities\n\n* May not dereferenced access values\n\n* Function calls and attribute references must be static""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Lock_Free""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Locking_Policy (policy_identifier);""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Locking_Policy""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Loop_Invariant ( boolean_EXPRESSION );\n\nThe effect of this pragma is similar to that of pragma `Assert', except\nthat in an `Assertion_Policy' pragma, the identifier `Loop_Invariant'\nis used to control whether it is ignored or checked (or disabled).\n\n`Loop_Invariant' can only appear as one of the items in the sequence of\nstatements of a loop body, or nested inside block statements that\nappear in the sequence of statements of a loop body.  The intention is\nthat it be used to represent a \""loop invariant\"" assertion, i.e.\nsomething that is true each time through the loop, and which can be\nused to show that the loop is achieving its purpose.\n\nMultiple `Loop_Invariant' and `Loop_Variant' pragmas that apply to the\nsame loop should be grouped in the same sequence of statements.\n\nTo aid in writing such invariants, the special attribute `Loop_Entry'\nmay be used to refer to the value of an expression on entry to the\nloop. This attribute can only be used within the expression of a\n`Loop_Invariant' pragma. For full details, see documentation of\nattribute `Loop_Entry'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Loop_Invariant""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Loop_Optimize (OPTIMIZATION_HINT {, OPTIMIZATION_HINT});\n\nOPTIMIZATION_HINT ::= Ivdep | No_Unroll | Unroll | No_Vector | Vector\n\nThis pragma must appear immediately within a loop statement.  It allows\nthe programmer to specify optimization hints for the enclosing loop.\nThe hints are not mutually exclusive and can be freely mixed, but not\nall combinations will yield a sensible outcome.\n\nThere are five supported optimization hints for a loop:\n\n* Ivdep\n\nThe programmer asserts that there are no loop-carried dependencies\nwhich would prevent consecutive iterations of the loop from being\nexecuted simultaneously.\n\n* No_Unroll\n\nThe loop must not be unrolled.  This is a strong hint: the\ncompiler will not unroll a loop marked with this hint.\n\n* Unroll\n\nThe loop should be unrolled.  This is a weak hint: the compiler\nwill try to apply unrolling to this loop preferably to other\noptimizations, notably vectorization, but there is no guarantee\nthat the loop will be unrolled.\n\n* No_Vector\n\nThe loop must not be vectorized.  This is a strong hint: the\ncompiler will not vectorize a loop marked with this hint.\n\n* Vector\n\nThe loop should be vectorized.  This is a weak hint: the compiler\nwill try to apply vectorization to this loop preferably to other\noptimizations, notably unrolling, but there is no guarantee that\nthe loop will be vectorized.\n\nThese hints do not remove the need to pass the appropriate switches to\nthe compiler in order to enable the relevant optimizations, that is to\nsay `-funroll-loops' for unrolling and `-ftree-vectorize' for\nvectorization.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Loop_Optimize""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Loop_Variant ( LOOP_VARIANT_ITEM {, LOOP_VARIANT_ITEM } );\nLOOP_VARIANT_ITEM ::= CHANGE_DIRECTION => discrete_EXPRESSION\nCHANGE_DIRECTION ::= Increases | Decreases\n\n`Loop_Variant' can only appear as one of the items in the sequence of\nstatements of a loop body, or nested inside block statements that\nappear in the sequence of statements of a loop body.  It allows the\nspecification of quantities which must always decrease or increase in\nsuccessive iterations of the loop. In its simplest form, just one\nexpression is specified, whose value must increase or decrease on each\niteration of the loop.\n\nIn a more complex form, multiple arguments can be given which are\nintepreted in a nesting lexicographic manner. For example:\n\npragma Loop_Variant (Increases => X, Decreases => Y);\n\nspecifies that each time through the loop either X increases, or X stays\nthe same and Y decreases. A `Loop_Variant' pragma ensures that the loop\nis making progress. It can be useful in helping to show informally or\nprove formally that the loop always terminates.\n\n`Loop_Variant' is an assertion whose effect can be controlled using an\n`Assertion_Policy' with a check name of `Loop_Variant'. The policy can\nbe `Check' to enable the loop variant check, `Ignore' to ignore the\ncheck (in which case the pragma has no effect on the program), or\n`Disable' in which case the pragma is not even checked for correct\nsyntax.\n\nMultiple `Loop_Invariant' and `Loop_Variant' pragmas that apply to the\nsame loop should be grouped in the same sequence of statements.\n\nThe `Loop_Entry' attribute may be used within the expressions of the\n`Loop_Variant' pragma to refer to values on entry to the loop.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Loop_Variant""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Machine_Attribute (\n[Entity         =>] LOCAL_NAME,\n[Attribute_Name =>] static_string_EXPRESSION\n[, [Info           =>] static_EXPRESSION {, static_EXPRESSION}] );\n\nMachine-dependent attributes can be specified for types and/or\ndeclarations.  This pragma is semantically equivalent to\n`__attribute__((`attribute_name'))' (if `info' is not specified) or\n`__attribute__((`attribute_name(info')))' or\n`__attribute__((`attribute_name(info,...')))' in GNU C, where\n`attribute_name' is recognized by the compiler middle-end or the\n`TARGET_ATTRIBUTE_TABLE' machine specific macro.  Note that a string\nliteral for the optional parameter `info' or the following ones is\ntransformed by default into an identifier, which may make this pragma\nunusable for some attributes.  For further information see `GNU\nCompiler Collection (GCC) Internals'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Machine_Attribute""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Main\n(MAIN_OPTION [, MAIN_OPTION]);\n\nMAIN_OPTION ::=\n[Stack_Size              =>] static_integer_EXPRESSION\n| [Task_Stack_Size_Default =>] static_integer_EXPRESSION\n| [Time_Slicing_Enabled    =>] static_boolean_EXPRESSION\n\nThis pragma is provided for compatibility with OpenVMS VAX Systems.  It\nhas no effect in GNAT, other than being syntax checked.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Main""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Main_Storage\n(MAIN_STORAGE_OPTION [, MAIN_STORAGE_OPTION]);\n\nMAIN_STORAGE_OPTION ::=\n[WORKING_STORAGE =>] static_SIMPLE_EXPRESSION\n| [TOP_GUARD       =>] static_SIMPLE_EXPRESSION\n\nThis pragma is provided for compatibility with OpenVMS VAX Systems.  It\nhas no effect in GNAT, other than being syntax checked.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Main_Storage""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Max_Entry_Queue (static_integer_EXPRESSION);\n\nThis pragma is used to specify the maximum callers per entry queue for\nindividual protected entries and entry families. It accepts a single\ninteger (-1 or more) as a parameter and must appear after the\ndeclaration of an entry.\n\nA value of -1 represents no additional restriction on queue length.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Max_Queue_Length""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma No_Body;\n\nThere are a number of cases in which a package spec does not require a\nbody, and in fact a body is not permitted. GNAT will not permit the\nspec to be compiled if there is a body around. The pragma No_Body\nallows you to provide a body file, even in a case where no body is\nallowed. The body file must contain only comments and a single No_Body\npragma. This is recognized by the compiler as indicating that no body\nis logically present.\n\nThis is particularly useful during maintenance when a package is\nmodified in such a way that a body needed before is no longer needed.\nThe provision of a dummy body with a No_Body pragma ensures that there\nis no interference from earlier versions of the package body.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Body""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma No_Caching [ (boolean_EXPRESSION) ];\n\nFor the semantics of this pragma, see the entry for aspect `No_Caching'\nin the SPARK 2014 Reference Manual, section 7.1.2.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Caching""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma No_Component_Reordering [([Entity =>] type_LOCAL_NAME)];\n\n`type_LOCAL_NAME' must refer to a record type declaration in the current\ndeclarative part. The effect is to preclude any reordering of components\nfor the layout of the record, i.e. the record is laid out by the\ncompiler in the order in which the components are declared textually.\nThe form with no argument is a configuration pragma which applies to\nall record types declared in units to which the pragma applies and\nthere is a requirement that this pragma be used consistently within a\npartition.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Component_Reordering""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma No_Elaboration_Code_All [(program_unit_NAME)];\n\nThis is a program unit pragma (there is also an equivalent aspect of the\nsame name) that establishes the restriction `No_Elaboration_Code' for\nthe current unit and any extended main source units (body and subunits).\nIt also has the effect of enforcing a transitive application of this\naspect, so that if any unit is implicitly or explicitly with'ed by the\ncurrent unit, it must also have the No_Elaboration_Code_All aspect set.\nIt may be applied to package or subprogram specs or their generic\nversions.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Elaboration_Code_All""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma No_Heap_Finalization [ (first_subtype_LOCAL_NAME) ];\n\nPragma `No_Heap_Finalization' may be used as a configuration pragma or\nas a type-specific pragma.\n\nIn its configuration form, the pragma must appear within a\nconfiguration file such as gnat.adc, without an argument. The pragma\nsuppresses the call to `Finalize' for heap-allocated objects created\nthrough library-level named access-to-object types in cases where the\ndesignated type requires finalization actions.\n\nIn its type-specific form, the argument of the pragma must denote a\nlibrary-level named access-to-object type. The pragma suppresses the\ncall to `Finalize' for heap-allocated objects created through the\nspecific access type in cases where the designated type requires\nfinalization actions.\n\nIt is still possible to finalize such heap-allocated objects by\nexplicitly deallocating them.\n\nA library-level named access-to-object type declared within a generic\nunit will lose its `No_Heap_Finalization' pragma when the corresponding\ninstance does not appear at the library level.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Heap_Finalization""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma No_Inline (NAME {, NAME});\n\nThis pragma suppresses inlining for the callable entity or the\ninstances of the generic subprogram designated by `NAME', including\ninlining that results from the use of pragma `Inline'.  This pragma is\nalways active, in particular it is not subject to the use of option\n`-gnatn' or `-gnatN'.  It is illegal to specify both pragma `No_Inline'\nand pragma `Inline_Always' for the same `NAME'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Inline""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma No_Return (procedure_LOCAL_NAME {, procedure_LOCAL_NAME});\n\nEach `procedure_LOCAL_NAME' argument must refer to one or more procedure\ndeclarations in the current declarative part.  A procedure to which this\npragma is applied may not contain any explicit `return' statements.  In\naddition, if the procedure contains any implicit returns from falling\noff the end of a statement sequence, then execution of that implicit\nreturn will cause Program_Error to be raised.\n\nOne use of this pragma is to identify procedures whose only purpose is\nto raise an exception. Another use of this pragma is to suppress\nincorrect warnings about missing returns in functions, where the last\nstatement of a function statement sequence is a call to such a\nprocedure.\n\nNote that in Ada 2005 mode, this pragma is part of the language. It is\navailable in all earlier versions of Ada as an implementation-defined\npragma.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Return""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma No_Strict_Aliasing [([Entity =>] type_LOCAL_NAME)];\n\n`type_LOCAL_NAME' must refer to an access type declaration in the\ncurrent declarative part.  The effect is to inhibit strict aliasing\noptimization for the given type.  The form with no arguments is a\nconfiguration pragma which applies to all access types declared in\nunits to which the pragma applies. For a detailed description of the\nstrict aliasing optimization, and the situations in which it must be\nsuppressed, see the section on Optimization and Strict Aliasing in the\n`GNAT User's Guide'.\n\nThis pragma currently has no effects on access to unconstrained array\ntypes.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Strict_Aliasing""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma No_Tagged_Streams [([Entity =>] tagged_type_LOCAL_NAME)];\n\nNormally when a tagged type is introduced using a full type declaration,\npart of the processing includes generating stream access routines to be\nused by stream attributes referencing the type (or one of its subtypes\nor derived types). This can involve the generation of significant\namounts of code which is wasted space if stream routines are not needed\nfor the type in question.\n\nThe `No_Tagged_Streams' pragma causes the generation of these stream\nroutines to be skipped, and any attempt to use stream operations on\ntypes subject to this pragma will be statically rejected as illegal.\n\nThere are two forms of the pragma. The form with no arguments must\nappear in a declarative sequence or in the declarations of a package\nspec. This pragma affects all subsequent root tagged types declared in\nthe declaration sequence, and specifies that no stream routines be\ngenerated. The form with an argument (for which there is also a\ncorresponding aspect) specifies a single root tagged type for which\nstream routines are not to be generated.\n\nOnce the pragma has been given for a particular root tagged type, all\nsubtypes and derived types of this type inherit the pragma\nautomatically, so the effect applies to a complete hierarchy (this is\nnecessary to deal with the class-wide dispatching versions of the\nstream routines).\n\nWhen pragmas `Discard_Names' and `No_Tagged_Streams' are simultaneously\napplied to a tagged type its Expanded_Name and External_Tag are\ninitialized with empty strings. This is useful to avoid exposing entity\nnames at binary level but has a negative impact on the debuggability of\ntagged types.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""No_Tagged_Streams""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Normalize_Scalars;\n\nThis is a language defined pragma which is fully implemented in GNAT.\nThe effect is to cause all scalar objects that are not otherwise\ninitialized to be initialized.  The initial values are implementation\ndependent and are as follows:\n\n`Standard.Character'\nObjects whose root type is Standard.Character are initialized to\nCharacter'Last unless the subtype range excludes NUL (in which case\nNUL is used). This choice will always generate an invalid value if\none exists.\n\n`Standard.Wide_Character'\nObjects whose root type is Standard.Wide_Character are initialized\nto Wide_Character'Last unless the subtype range excludes NUL (in\nwhich case NUL is used). This choice will always generate an\ninvalid value if one exists.\n\n`Standard.Wide_Wide_Character'\nObjects whose root type is Standard.Wide_Wide_Character are\ninitialized to the invalid value 16#FFFF_FFFF# unless the subtype\nrange excludes NUL (in which case NUL is used). This choice will\nalways generate an invalid value if one exists.\n\n`Integer types'\nObjects of an integer type are treated differently depending on\nwhether negative values are present in the subtype. If no negative\nvalues are present, then all one bits is used as the initial value\nexcept in the special case where zero is excluded from the\nsubtype, in which case all zero bits are used. This choice will\nalways generate an invalid value if one exists.\n\nFor subtypes with negative values present, the largest negative\nnumber is used, except in the unusual case where this largest\nnegative number is in the subtype, and the largest positive number\nis not, in which case the largest positive value is used. This\nchoice will always generate an invalid value if one exists.\n\n`Floating-Point Types'\nObjects of all floating-point types are initialized to all 1-bits.\nFor standard IEEE format, this corresponds to a NaN (not a number)\nwhich is indeed an invalid value.\n\n`Fixed-Point Types'\nObjects of all fixed-point types are treated as described above\nfor integers, with the rules applying to the underlying integer\nvalue used to represent the fixed-point value.\n\n`Modular types'\nObjects of a modular type are initialized to all one bits, except\nin the special case where zero is excluded from the subtype, in\nwhich case all zero bits are used. This choice will always\ngenerate an invalid value if one exists.\n\n`Enumeration types'\nObjects of an enumeration type are initialized to all one-bits,\ni.e., to the value `2 ** typ'Size - 1' unless the subtype excludes\nthe literal whose Pos value is zero, in which case a code of zero\nis used. This choice will always generate an invalid value if one\nexists.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Normalize_Scalars""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Obsolescent;\n\npragma Obsolescent (\n[Message =>] static_string_EXPRESSION\n[,[Version =>] Ada_05]]);\n\npragma Obsolescent (\n[Entity  =>] NAME\n[,[Message =>] static_string_EXPRESSION\n[,[Version =>] Ada_05]] );\n\nThis pragma can occur immediately following a declaration of an entity,\nincluding the case of a record component. If no Entity argument is\npresent, then this declaration is the one to which the pragma applies.\nIf an Entity parameter is present, it must either match the name of the\nentity in this declaration, or alternatively, the pragma can\nimmediately follow an enumeration type declaration, where the Entity\nargument names one of the enumeration literals.\n\nThis pragma is used to indicate that the named entity is considered\nobsolescent and should not be used. Typically this is used when an API\nmust be modified by eventually removing or modifying existing\nsubprograms or other entities. The pragma can be used at an\nintermediate stage when the entity is still present, but will be\nremoved later.\n\nThe effect of this pragma is to output a warning message on a reference\nto an entity thus marked that the subprogram is obsolescent if the\nappropriate warning option in the compiler is activated. If the\n`Message' parameter is present, then a second warning message is given\ncontaining this text. In addition, a reference to the entity is\nconsidered to be a violation of pragma `Restrictions\n(No_Obsolescent_Features)'.\n\nThis pragma can also be used as a program unit pragma for a package, in\nwhich case the entity name is the name of the package, and the pragma\nindicates that the entire package is considered obsolescent. In this\ncase a client `with'ing such a package violates the restriction, and\nthe `with' clause is flagged with warnings if the warning option is set.\n\nIf the `Version' parameter is present (which must be exactly the\nidentifier `Ada_05', no other argument is allowed), then the indication\nof obsolescence applies only when compiling in Ada 2005 mode. This is\nprimarily intended for dealing with the situations in the predefined\nlibrary where subprograms or packages have become defined as\nobsolescent in Ada 2005 (e.g., in `Ada.Characters.Handling'), but may\nbe used anywhere.\n\nThe following examples show typical uses of this pragma:\n\npackage p is\npragma Obsolescent (p, Message => \""use pp instead of p\"");\nend p;\n\npackage q is\nprocedure q2;\npragma Obsolescent (\""use q2new instead\"");\n\ntype R is new integer;\npragma Obsolescent\n(Entity  => R,\nMessage => \""use RR in Ada 2005\"",\nVersion => Ada_05);\n\ntype M is record\nF1 : Integer;\nF2 : Integer;\npragma Obsolescent;\nF3 : Integer;\nend record;\n\ntype E is (a, bc, 'd', quack);\npragma Obsolescent (Entity => bc)\npragma Obsolescent (Entity => 'd')\n\nfunction \""+\""\n(a, b : character) return character;\npragma Obsolescent (Entity => \""+\"");\nend;\n\nNote that, as for all pragmas, if you use a pragma argument identifier,\nthen all subsequent parameters must also use a pragma argument\nidentifier.  So if you specify `Entity =>' for the `Entity' argument,\nand a `Message' argument is present, it must be preceded by `Message\n=>'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Obsolescent""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Optimize (identifier);""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Optimize""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Optimize_Alignment (TIME | SPACE | OFF);\n\nThis is a configuration pragma which affects the choice of default\nalignments for types and objects where no alignment is explicitly\nspecified. There is a time/space trade-off in the selection of these\nvalues. Large alignments result in more efficient code, at the expense\nof larger data space, since sizes have to be increased to match these\nalignments. Smaller alignments save space, but the access code is\nslower. The normal choice of default alignments for types and\nindividual alignment promotions for objects (which is what you get if\nyou do not use this pragma, or if you use an argument of OFF), tries to\nbalance these two requirements.\n\nSpecifying SPACE causes smaller default alignments to be chosen in two\ncases.  First any packed record is given an alignment of 1. Second, if\na size is given for the type, then the alignment is chosen to avoid\nincreasing this size. For example, consider:\n\ntype R is record\nX : Integer;\nY : Character;\nend record;\n\nfor R'Size use 5*8;\n\nIn the default mode, this type gets an alignment of 4, so that access\nto the Integer field X are efficient. But this means that objects of\nthe type end up with a size of 8 bytes. This is a valid choice, since\nsizes of objects are allowed to be bigger than the size of the type,\nbut it can waste space if for example fields of type R appear in an\nenclosing record. If the above type is compiled in `Optimize_Alignment\n(Space)' mode, the alignment is set to 1.\n\nHowever, there is one case in which SPACE is ignored. If a variable\nlength record (that is a discriminated record with a component which is\nan array whose length depends on a discriminant), has a pragma Pack,\nthen it is not in general possible to set the alignment of such a\nrecord to one, so the pragma is ignored in this case (with a warning).\n\nSpecifying SPACE also disables alignment promotions for standalone\nobjects, which occur when the compiler increases the alignment of a\nspecific object without changing the alignment of its type.\n\nSpecifying SPACE also disables component reordering in unpacked record\ntypes, which can result in larger sizes in order to meet alignment\nrequirements.\n\nSpecifying TIME causes larger default alignments to be chosen in the\ncase of small types with sizes that are not a power of 2. For example,\nconsider:\n\ntype R is record\nA : Character;\nB : Character;\nC : Boolean;\nend record;\n\npragma Pack (R);\nfor R'Size use 17;\n\nThe default alignment for this record is normally 1, but if this type is\ncompiled in `Optimize_Alignment (Time)' mode, then the alignment is set\nto 4, which wastes space for objects of the type, since they are now 4\nbytes long, but results in more efficient access when the whole record\nis referenced.\n\nAs noted above, this is a configuration pragma, and there is a\nrequirement that all units in a partition be compiled with a consistent\nsetting of the optimization setting. This would normally be achieved by\nuse of a configuration pragma file containing the appropriate setting.\nThe exception to this rule is that units with an explicit configuration\npragma in the same file as the source unit are excluded from the\nconsistency check, as are all predefined units. The latter are compiled\nby default in pragma Optimize_Alignment (Off) mode if no pragma appears\nat the start of the file.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Optimize_Alignment""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Ordered (enumeration_first_subtype_LOCAL_NAME);\n\nMost enumeration types are from a conceptual point of view unordered.\nFor example, consider:\n\ntype Color is (Red, Blue, Green, Yellow);\n\nBy Ada semantics `Blue > Red' and `Green > Blue', but really these\nrelations make no sense; the enumeration type merely specifies a set of\npossible colors, and the order is unimportant.\n\nFor unordered enumeration types, it is generally a good idea if clients\navoid comparisons (other than equality or inequality) and explicit\nranges. (A `client' is a unit where the type is referenced, other than\nthe unit where the type is declared, its body, and its subunits.)  For\nexample, if code buried in some client says:\n\nif Current_Color < Yellow then ...\nif Current_Color in Blue .. Green then ...\n\nthen the client code is relying on the order, which is undesirable.  It\nmakes the code hard to read and creates maintenance difficulties if\nentries have to be added to the enumeration type. Instead, the code in\nthe client should list the possibilities, or an appropriate subtype\nshould be declared in the unit that declares the original enumeration\ntype. E.g., the following subtype could be declared along with the type\n`Color':\n\nsubtype RBG is Color range Red .. Green;\n\nand then the client could write:\n\nif Current_Color in RBG then ...\nif Current_Color = Blue or Current_Color = Green then ...\n\nHowever, some enumeration types are legitimately ordered from a\nconceptual point of view. For example, if you declare:\n\ntype Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);\n\nthen the ordering imposed by the language is reasonable, and clients\ncan depend on it, writing for example:\n\nif D in Mon .. Fri then ...\nif D < Wed then ...\n\nThe pragma `Ordered' is provided to mark enumeration types that are\nconceptually ordered, alerting the reader that clients may depend on\nthe ordering. GNAT provides a pragma to mark enumerations as ordered\nrather than one to mark them as unordered, since in our experience, the\ngreat majority of enumeration types are conceptually unordered.\n\nThe types `Boolean', `Character', `Wide_Character', and\n`Wide_Wide_Character' are considered to be ordered types, so each is\ndeclared with a pragma `Ordered' in package `Standard'.\n\nNormally pragma `Ordered' serves only as documentation and a guide for\ncoding standards, but GNAT provides a warning switch `-gnatw.u' that\nrequests warnings for inappropriate uses (comparisons and explicit\nsubranges) for unordered types. If this switch is used, then any\nenumeration type not marked with pragma `Ordered' will be considered as\nunordered, and will generate warnings for inappropriate uses.\n\nNote that generic types are not considered ordered or unordered (since\nthe template can be instantiated for both cases), so we never generate\nwarnings for the case of generic enumerated types.\n\nFor additional information please refer to the description of the\n`-gnatw.u' switch in the GNAT User's Guide.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Ordered""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Overflow_Mode\n(  [General    =>] MODE\n[,[Assertions =>] MODE]);\n\nMODE ::= STRICT | MINIMIZED | ELIMINATED\n\nThis pragma sets the current overflow mode to the given setting. For\ndetails of the meaning of these modes, please refer to the 'Overflow\nCheck Handling in GNAT' appendix in the GNAT User's Guide. If only the\n`General' parameter is present, the given mode applies to all\nexpressions. If both parameters are present, the `General' mode applies\nto expressions outside assertions, and the `Eliminated' mode applies to\nexpressions within assertions.\n\nThe case of the `MODE' parameter is ignored, so `MINIMIZED',\n`Minimized' and `minimized' all have the same effect.\n\nThe `Overflow_Mode' pragma has the same scoping and placement rules as\npragma `Suppress', so it can occur either as a configuration pragma,\nspecifying a default for the whole program, or in a declarative scope,\nwhere it applies to the remaining declarations and statements in that\nscope.\n\nThe pragma `Suppress (Overflow_Check)' suppresses overflow checking,\nbut does not affect the overflow mode.\n\nThe pragma `Unsuppress (Overflow_Check)' unsuppresses (enables)\noverflow checking, but does not affect the overflow mode.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Overflow_Mode""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Overriding_Renamings;\n\nThis is a GNAT configuration pragma to simplify porting legacy code\naccepted by the Rational Ada compiler. In the presence of this pragma,\na renaming declaration that renames an inherited operation declared in\nthe same scope is legal if selected notation is used as in:\n\npragma Overriding_Renamings;\n\npackage R is\nfunction F (..);\n\nfunction F (..) renames R.F;\nend R;\n\neven though RM 8.3 (15) stipulates that an overridden operation is not\nvisible within the declaration of the overriding operation.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Overriding_Renamings""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Pack;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Pack""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Page;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Page""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Part_Of (ABSTRACT_STATE);\n\nABSTRACT_STATE ::= NAME\n\nFor the semantics of this pragma, see the entry for aspect `Part_Of' in\nthe SPARK 2014 Reference Manual, section 7.2.6.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Part_Of""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Partition_Elaboration_Policy (POLICY_IDENTIFIER);\n\nPOLICY_IDENTIFIER ::= Concurrent | Sequential\n\nThis pragma is standard in Ada 2005, but is available in all earlier\nversions of Ada as an implementation-defined pragma.  See Ada 2012\nReference Manual for details.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Partition_Elaboration_Policy""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Passive [(Semaphore | No)];\n\nSyntax checked, but otherwise ignored by GNAT.  This is recognized for\ncompatibility with DEC Ada 83 implementations, where it is used within a\ntask definition to request that a task be made passive.  If the argument\n`Semaphore' is present, or the argument is omitted, then DEC Ada 83\ntreats the pragma as an assertion that the containing task is passive\nand that optimization of context switch with this task is permitted and\ndesired.  If the argument `No' is present, the task must not be\noptimized.  GNAT does not attempt to optimize any tasks in this manner\n(since protected objects are available in place of passive tasks).\n\nFor more information on the subject of passive tasks, see the section\n'Passive Task Optimization' in the GNAT Users Guide.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Passive""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Persistent_BSS [(LOCAL_NAME)]\n\nThis pragma allows selected objects to be placed in the\n`.persistent_bss' section. On some targets the linker and loader\nprovide for special treatment of this section, allowing a program to be\nreloaded without affecting the contents of this data (hence the name\npersistent).\n\nThere are two forms of usage. If an argument is given, it must be the\nlocal name of a library-level object, with no explicit initialization\nand whose type is potentially persistent. If no argument is given, then\nthe pragma is a configuration pragma, and applies to all library-level\nobjects with no explicit initialization of potentially persistent types.\n\nA potentially persistent type is a scalar type, or an untagged,\nnon-discriminated record, all of whose components have no explicit\ninitialization and are themselves of a potentially persistent type, or\nan array, all of whose constraints are static, and whose component type\nis potentially persistent.\n\nIf this pragma is used on a target where this feature is not supported,\nthen the pragma will be ignored. See also `pragma Linker_Section'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Persistent_BSS""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Polling (ON | OFF);\n\nThis pragma controls the generation of polling code.  This is normally\noff.  If `pragma Polling (ON)' is used then periodic calls are\ngenerated to the routine `Ada.Exceptions.Poll'.  This routine is a\nseparate unit in the runtime library, and can be found in file\n`a-excpol.adb'.\n\nPragma `Polling' can appear as a configuration pragma (for example it\ncan be placed in the `gnat.adc' file) to enable polling globally, or it\ncan be used in the statement or declaration sequence to control polling\nmore locally.\n\nA call to the polling routine is generated at the start of every loop\nand at the start of every subprogram call.  This guarantees that the\n`Poll' routine is called frequently, and places an upper bound\n(determined by the complexity of the code) on the period between two\n`Poll' calls.\n\nThe primary purpose of the polling interface is to enable asynchronous\naborts on targets that cannot otherwise support it (for example Windows\nNT), but it may be used for any other purpose requiring periodic\npolling.  The standard version is null, and can be replaced by a user\nprogram.  This will require re-compilation of the `Ada.Exceptions'\npackage that can be found in files `a-except.ads' and `a-except.adb'.\n\nA standard alternative unit (in file `4wexcpol.adb' in the standard GNAT\ndistribution) is used to enable the asynchronous abort capability on\ntargets that do not normally support the capability.  The version of\n`Poll' in this file makes a call to the appropriate runtime routine to\ntest for an abort condition.\n\nNote that polling can also be enabled by use of the `-gnatP' switch.\nSee the section on switches for gcc in the `GNAT User's Guide'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Polling""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Post (Boolean_Expression);\n\nThe `Post' pragma is intended to be an exact replacement for the\nlanguage-defined `Post' aspect, and shares its restrictions and\nsemantics.  It must appear either immediately following the\ncorresponding subprogram declaration (only other pragmas may\nintervene), or if there is no separate subprogram declaration, then it\ncan appear at the start of the declarations in a subprogram body\n(preceded only by other pragmas).""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Post""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Post_Class (Boolean_Expression);\n\nThe `Post_Class' pragma is intended to be an exact replacement for the\nlanguage-defined `Post'Class' aspect, and shares its restrictions and\nsemantics.  It must appear either immediately following the\ncorresponding subprogram declaration (only other pragmas may\nintervene), or if there is no separate subprogram declaration, then it\ncan appear at the start of the declarations in a subprogram body\n(preceded only by other pragmas).\n\nNote: This pragma is called `Post_Class' rather than `Post'Class'\nbecause the latter would not be strictly conforming to the allowed\nsyntax for pragmas. The motivation for provinding pragmas equivalent to\nthe aspects is to allow a program to be written using the pragmas, and\nthen compiled if necessary using an Ada compiler that does not\nrecognize the pragmas or aspects, but is prepared to ignore the\npragmas. The assertion policy that controls this pragma is\n`Post'Class', not `Post_Class'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Post_Class""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Postcondition (\n[Check   =>] Boolean_Expression\n[,[Message =>] String_Expression]);\n\nThe `Postcondition' pragma allows specification of automatic\npostcondition checks for subprograms. These checks are similar to\nassertions, but are automatically inserted just prior to the return\nstatements of the subprogram with which they are associated (including\nimplicit returns at the end of procedure bodies and associated\nexception handlers).\n\nIn addition, the boolean expression which is the condition which must\nbe true may contain references to function'Result in the case of a\nfunction to refer to the returned value.\n\n`Postcondition' pragmas may appear either immediately following the\n(separate) declaration of a subprogram, or at the start of the\ndeclarations of a subprogram body. Only other pragmas may intervene\n(that is appear between the subprogram declaration and its\npostconditions, or appear before the postcondition in the declaration\nsequence in a subprogram body). In the case of a postcondition\nappearing after a subprogram declaration, the formal arguments of the\nsubprogram are visible, and can be referenced in the postcondition\nexpressions.\n\nThe postconditions are collected and automatically tested just before\nany return (implicit or explicit) in the subprogram body.  A\npostcondition is only recognized if postconditions are active at the\ntime the pragma is encountered. The compiler switch `gnata' turns on\nall postconditions by default, and pragma `Check_Policy' with an\nidentifier of `Postcondition' can also be used to control whether\npostconditions are active.\n\nThe general approach is that postconditions are placed in the spec if\nthey represent functional aspects which make sense to the client.  For\nexample we might have:\n\nfunction Direction return Integer;\npragma Postcondition\n(Direction'Result = +1\nor else\nDirection'Result = -1);\n\nwhich serves to document that the result must be +1 or -1, and will\ntest that this is the case at run time if postcondition checking is\nactive.\n\nPostconditions within the subprogram body can be used to check that\nsome internal aspect of the implementation, not visible to the client,\nis operating as expected.  For instance if a square root routine keeps\nan internal counter of the number of times it is called, then we might\nhave the following postcondition:\n\nSqrt_Calls : Natural := 0;\n\nfunction Sqrt (Arg : Float) return Float is\npragma Postcondition\n(Sqrt_Calls = Sqrt_Calls'Old + 1);\n\nend Sqrt\n\nAs this example, shows, the use of the `Old' attribute is often useful\nin postconditions to refer to the state on entry to the subprogram.\n\nNote that postconditions are only checked on normal returns from the\nsubprogram. If an abnormal return results from raising an exception,\nthen the postconditions are not checked.\n\nIf a postcondition fails, then the exception\n`System.Assertions.Assert_Failure' is raised. If a message argument was\nsupplied, then the given string will be used as the exception message.\nIf no message argument was supplied, then the default message has the\nform \""Postcondition failed at file_name:line\"". The exception is raised\nin the context of the subprogram body, so it is possible to catch\npostcondition failures within the subprogram body itself.\n\nWithin a package spec, normal visibility rules in Ada would prevent\nforward references within a postcondition pragma to functions defined\nlater in the same package. This would introduce undesirable ordering\nconstraints. To avoid this problem, all postcondition pragmas are\nanalyzed at the end of the package spec, allowing forward references.\n\nThe following example shows that this even allows mutually recursive\npostconditions as in:\n\npackage Parity_Functions is\nfunction Odd  (X : Natural) return Boolean;\npragma Postcondition\n(Odd'Result =\n(x = 1\nor else\n(x /= 0 and then Even (X - 1))));\n\nfunction Even (X : Natural) return Boolean;\npragma Postcondition\n(Even'Result =\n(x = 0\nor else\n(x /= 1 and then Odd (X - 1))));\n\nend Parity_Functions;\n\nThere are no restrictions on the complexity or form of conditions used\nwithin `Postcondition' pragmas.  The following example shows that it is\neven possible to verify performance behavior.\n\npackage Sort is\n\nPerformance : constant Float;\n--  Performance constant set by implementation\n--  to match target architecture behavior.\n\nprocedure Treesort (Arg : String);\n--  Sorts characters of argument using N*logN sort\npragma Postcondition\n(Float (Clock - Clock'Old) <=\nFloat (Arg'Length) *\nlog (Float (Arg'Length)) *\nPerformance);\nend Sort;\n\nNote: postcondition pragmas associated with subprograms that are marked\nas Inline_Always, or those marked as Inline with front-end inlining\n(-gnatN option set) are accepted and legality-checked by the compiler,\nbut are ignored at run-time even if postcondition checking is enabled.\n\nNote that pragma `Postcondition' differs from the language-defined\n`Post' aspect (and corresponding `Post' pragma) in allowing multiple\noccurrences, allowing occurences in the body even if there is a\nseparate spec, and allowing a second string parameter, and the use of\nthe pragma identifier `Check'. Historically, pragma `Postcondition' was\nimplemented prior to the development of Ada 2012, and has been retained\nin its original form for compatibility purposes.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Postcondition""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Pre (Boolean_Expression);\n\nThe `Pre' pragma is intended to be an exact replacement for the\nlanguage-defined `Pre' aspect, and shares its restrictions and\nsemantics.  It must appear either immediately following the\ncorresponding subprogram declaration (only other pragmas may\nintervene), or if there is no separate subprogram declaration, then it\ncan appear at the start of the declarations in a subprogram body\n(preceded only by other pragmas).""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Pre""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Pre_Class (Boolean_Expression);\n\nThe `Pre_Class' pragma is intended to be an exact replacement for the\nlanguage-defined `Pre'Class' aspect, and shares its restrictions and\nsemantics.  It must appear either immediately following the\ncorresponding subprogram declaration (only other pragmas may\nintervene), or if there is no separate subprogram declaration, then it\ncan appear at the start of the declarations in a subprogram body\n(preceded only by other pragmas).\n\nNote: This pragma is called `Pre_Class' rather than `Pre'Class' because\nthe latter would not be strictly conforming to the allowed syntax for\npragmas. The motivation for providing pragmas equivalent to the aspects\nis to allow a program to be written using the pragmas, and then\ncompiled if necessary using an Ada compiler that does not recognize the\npragmas or aspects, but is prepared to ignore the pragmas. The assertion\npolicy that controls this pragma is `Pre'Class', not `Pre_Class'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Pre_Class""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Precondition (\n[Check   =>] Boolean_Expression\n[,[Message =>] String_Expression]);\n\nThe `Precondition' pragma is similar to `Postcondition' except that the\ncorresponding checks take place immediately upon entry to the\nsubprogram, and if a precondition fails, the exception is raised in the\ncontext of the caller, and the attribute 'Result cannot be used within\nthe precondition expression.\n\nOtherwise, the placement and visibility rules are identical to those\ndescribed for postconditions. The following is an example of use within\na package spec:\n\npackage Math_Functions is\n\nfunction Sqrt (Arg : Float) return Float;\npragma Precondition (Arg >= 0.0)\n\nend Math_Functions;\n\n`Precondition' pragmas may appear either immediately following the\n(separate) declaration of a subprogram, or at the start of the\ndeclarations of a subprogram body. Only other pragmas may intervene\n(that is appear between the subprogram declaration and its\npostconditions, or appear before the postcondition in the declaration\nsequence in a subprogram body).\n\nNote: precondition pragmas associated with subprograms that are marked\nas Inline_Always, or those marked as Inline with front-end inlining\n(-gnatN option set) are accepted and legality-checked by the compiler,\nbut are ignored at run-time even if precondition checking is enabled.\n\nNote that pragma `Precondition' differs from the language-defined `Pre'\naspect (and corresponding `Pre' pragma) in allowing multiple\noccurrences, allowing occurences in the body even if there is a\nseparate spec, and allowing a second string parameter, and the use of\nthe pragma identifier `Check'. Historically, pragma `Precondition' was\nimplemented prior to the development of Ada 2012, and has been retained\nin its original form for compatibility purposes.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Precondition""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Predicate\n([Entity =>] type_LOCAL_NAME,\n[Check  =>] EXPRESSION);\n\nThis pragma (available in all versions of Ada in GNAT) encompasses both\nthe `Static_Predicate' and `Dynamic_Predicate' aspects in Ada 2012. A\npredicate is regarded as static if it has an allowed form for\n`Static_Predicate' and is otherwise treated as a `Dynamic_Predicate'.\nOtherwise, predicates specified by this pragma behave exactly as\ndescribed in the Ada 2012 reference manual.  For example, if we have\n\ntype R is range 1 .. 10;\nsubtype S is R;\npragma Predicate (Entity => S, Check => S not in 4 .. 6);\nsubtype Q is R\npragma Predicate (Entity => Q, Check => F(Q) or G(Q));\n\nthe effect is identical to the following Ada 2012 code:\n\ntype R is range 1 .. 10;\nsubtype S is R with\nStatic_Predicate => S not in 4 .. 6;\nsubtype Q is R with\nDynamic_Predicate => F(Q) or G(Q);\n\nNote that there are no pragmas `Dynamic_Predicate' or\n`Static_Predicate'. That is because these pragmas would affect legality\nand semantics of the program and thus do not have a neutral effect if\nignored.  The motivation behind providing pragmas equivalent to\ncorresponding aspects is to allow a program to be written using the\npragmas, and then compiled with a compiler that will ignore the\npragmas. That doesn't work in the case of static and dynamic\npredicates, since if the corresponding pragmas are ignored, then the\nbehavior of the program is fundamentally changed (for example a\nmembership test `A in B' would not take into account a predicate\ndefined for subtype B). When following this approach, the use of\npredicates should be avoided.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Predicate""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Predicate_Failure\n([Entity  =>] type_LOCAL_NAME,\n[Message =>] String_Expression);\n\nThe `Predicate_Failure' pragma is intended to be an exact replacement\nfor the language-defined `Predicate_Failure' aspect, and shares its\nrestrictions and semantics.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Predicate_Failure""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Preelaborable_Initialization (DIRECT_NAME);\n\nThis pragma is standard in Ada 2005, but is available in all earlier\nversions of Ada as an implementation-defined pragma.  See Ada 2012\nReference Manual for details.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Preelaborable_Initialization""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Preelaborate [(library_unit_name)];""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Preelaborate""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Prefix_Exception_Messages;\n\nThis is an implementation-defined configuration pragma that affects the\nbehavior of raise statements with a message given as a static string\nconstant (typically a string literal). In such cases, the string will\nbe automatically prefixed by the name of the enclosing entity (giving\nthe package and subprogram containing the raise statement). This helps\nto identify where messages are coming from, and this mode is automatic\nfor the run-time library.\n\nThe pragma has no effect if the message is computed with an expression\nother than a static string constant, since the assumption in this case\nis that the program computes exactly the string it wants. If you still\nwant the prefixing in this case, you can always call\n`GNAT.Source_Info.Enclosing_Entity' and prepend the string manually.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Prefix_Exception_Messages""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Priority;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Priority""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Priority_Specific_Dispatching (\nPOLICY_IDENTIFIER,\nfirst_priority_EXPRESSION,\nlast_priority_EXPRESSION)\n\nPOLICY_IDENTIFIER ::=\nEDF_Across_Priorities            |\nFIFO_Within_Priorities           |\nNon_Preemptive_Within_Priorities |\nRound_Robin_Within_Priorities\n\nThis pragma is standard in Ada 2005, but is available in all earlier\nversions of Ada as an implementation-defined pragma.  See Ada 2012\nReference Manual for details.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Priority_Specific_Dispatching""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Profile (Ravenscar | Restricted | Rational |\nGNAT_Extended_Ravenscar | GNAT_Ravenscar_EDF );\n\nThis pragma is standard in Ada 2005, but is available in all earlier\nversions of Ada as an implementation-defined pragma. This is a\nconfiguration pragma that establishes a set of configuration pragmas\nthat depend on the argument. `Ravenscar' is standard in Ada 2005.  The\nother possibilities (`Restricted', `Rational',\n`GNAT_Extended_Ravenscar', `GNAT_Ravenscar_EDF') are\nimplementation-defined. The set of configuration pragmas is defined in\nthe following sections.\n\n* Pragma Profile (Ravenscar)\n\nThe `Ravenscar' profile is standard in Ada 2005, but is available\nin all earlier versions of Ada as an implementation-defined\npragma. This profile establishes the following set of\nconfiguration pragmas:\n\n* `Task_Dispatching_Policy (FIFO_Within_Priorities)'\n\n[RM D.2.2] Tasks are dispatched following a preemptive\npriority-ordered scheduling policy.\n\n* `Locking_Policy (Ceiling_Locking)'\n\n[RM D.3] While tasks and interrupts execute a protected\naction, they inherit the ceiling priority of the\ncorresponding protected object.\n\n* `Detect_Blocking'\n\nThis pragma forces the detection of potentially blocking\noperations within a protected operation, and to raise\nProgram_Error if that happens.\n\nplus the following set of restrictions:\n\n* `Max_Entry_Queue_Length => 1'\n\nNo task can be queued on a protected entry.\n\n* `Max_Protected_Entries => 1'\n\n* `Max_Task_Entries => 0'\n\nNo rendezvous statements are allowed.\n\n* `No_Abort_Statements'\n\n* `No_Dynamic_Attachment'\n\n* `No_Dynamic_Priorities'\n\n* `No_Implicit_Heap_Allocations'\n\n* `No_Local_Protected_Objects'\n\n* `No_Local_Timing_Events'\n\n* `No_Protected_Type_Allocators'\n\n* `No_Relative_Delay'\n\n* `No_Requeue_Statements'\n\n* `No_Select_Statements'\n\n* `No_Specific_Termination_Handlers'\n\n* `No_Task_Allocators'\n\n* `No_Task_Hierarchy'\n\n* `No_Task_Termination'\n\n* `Simple_Barriers'\n\nThe Ravenscar profile also includes the following restrictions\nthat specify that there are no semantic dependences on the\ncorresponding predefined packages:\n\n* `No_Dependence => Ada.Asynchronous_Task_Control'\n\n* `No_Dependence => Ada.Calendar'\n\n* `No_Dependence => Ada.Execution_Time.Group_Budget'\n\n* `No_Dependence => Ada.Execution_Time.Timers'\n\n* `No_Dependence => Ada.Task_Attributes'\n\n* `No_Dependence => System.Multiprocessors.Dispatching_Domains'\n\nThis set of configuration pragmas and restrictions correspond to\nthe definition of the 'Ravenscar Profile' for limited tasking,\ndevised and published by the `International Real-Time Ada\nWorkshop, 1997'.  A description is also available at\n<http://www-users.cs.york.ac.uk/~burns/ravenscar.ps>.\n\nThe original definition of the profile was revised at subsequent\nIRTAW meetings. It has been included in the ISO `Guide for the Use\nof the Ada Programming Language in High Integrity Systems', and\nwas made part of the Ada 2005 standard.  The formal definition\ngiven by the Ada Rapporteur Group (ARG) can be found in two Ada\nIssues (AI-249 and AI-305) available at\n<http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ais/ai-00249.txt> and\n<http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ais/ai-00305.txt>.\n\nThe above set is a superset of the restrictions provided by pragma\n`Profile (Restricted)', it includes six additional restrictions\n(`Simple_Barriers', `No_Select_Statements', `No_Calendar',\n`No_Implicit_Heap_Allocations', `No_Relative_Delay' and\n`No_Task_Termination').  This means that pragma `Profile\n(Ravenscar)', like the pragma `Profile (Restricted)',\nautomatically causes the use of a simplified, more efficient\nversion of the tasking run-time library.\n\n* Pragma Profile (GNAT_Extended_Ravenscar)\n\nThis profile corresponds to a GNAT specific extension of the\nRavenscar profile. The profile may change in the future although\nonly in a compatible way: some restrictions may be removed or\nrelaxed. It is defined as a variation of the Ravenscar profile.\n\nThe `No_Implicit_Heap_Allocations' restriction has been replaced\nby `No_Implicit_Task_Allocations' and\n`No_Implicit_Protected_Object_Allocations'.\n\nThe `Simple_Barriers' restriction has been replaced by\n`Pure_Barriers'.\n\nThe `Max_Protected_Entries', `Max_Entry_Queue_Length', and\n`No_Relative_Delay' restrictions have been removed.\n\n* Pragma Profile (GNAT_Ravenscar_EDF)\n\nThis profile corresponds to the Ravenscar profile but using\nEDF_Across_Priority as the Task_Scheduling_Policy.\n\n* Pragma Profile (Restricted)\n\nThis profile corresponds to the GNAT restricted run time. It\nestablishes the following set of restrictions:\n\n* `No_Abort_Statements'\n\n* `No_Entry_Queue'\n\n* `No_Task_Hierarchy'\n\n* `No_Task_Allocators'\n\n* `No_Dynamic_Priorities'\n\n* `No_Terminate_Alternatives'\n\n* `No_Dynamic_Attachment'\n\n* `No_Protected_Type_Allocators'\n\n* `No_Local_Protected_Objects'\n\n* `No_Requeue_Statements'\n\n* `No_Task_Attributes_Package'\n\n* `Max_Asynchronous_Select_Nesting =  0'\n\n* `Max_Task_Entries =  0'\n\n* `Max_Protected_Entries = 1'\n\n* `Max_Select_Alternatives = 0'\n\nThis set of restrictions causes the automatic selection of a\nsimplified version of the run time that provides improved\nperformance for the limited set of tasking functionality permitted\nby this set of restrictions.\n\n* Pragma Profile (Rational)\n\nThe Rational profile is intended to facilitate porting legacy code\nthat compiles with the Rational APEX compiler, even when the code\nincludes non- conforming Ada constructs.  The profile enables the\nfollowing three pragmas:\n\n* `pragma Implicit_Packing'\n\n* `pragma Overriding_Renamings'\n\n* `pragma Use_VADS_Size'""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Profile""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Profile_Warnings (Ravenscar | Restricted | Rational);\n\nThis is an implementation-defined pragma that is similar in effect to\n`pragma Profile' except that instead of generating `Restrictions'\npragmas, it generates `Restriction_Warnings' pragmas. The result is that\nviolations of the profile generate warning messages instead of error\nmessages.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Profile_Warnings""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Propagate_Exceptions;\n\nThis pragma is now obsolete and, other than generating a warning if\nwarnings on obsolescent features are enabled, is ignored.  It is\nretained for compatibility purposes. It used to be used in connection\nwith optimization of a now-obsolete mechanism for implementation of\nexceptions.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Propagate_Exceptions""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Provide_Shift_Operators (integer_first_subtype_LOCAL_NAME);\n\nThis pragma can be applied to a first subtype local name that specifies\neither an unsigned or signed type. It has the effect of providing the\nfive shift operators (Shift_Left, Shift_Right, Shift_Right_Arithmetic,\nRotate_Left and Rotate_Right) for the given type. It is similar to\nincluding the function declarations for these five operators, together\nwith the pragma Import (Intrinsic, ...) statements.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Provide_Shift_Operators""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Psect_Object (\n[Internal =>] LOCAL_NAME,\n[, [External =>] EXTERNAL_SYMBOL]\n[, [Size     =>] EXTERNAL_SYMBOL]);\n\nEXTERNAL_SYMBOL ::=\nIDENTIFIER\n| static_string_EXPRESSION\n\nThis pragma is identical in effect to pragma `Common_Object'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Psect_Object""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Pure [(library_unit_name)];""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Pure""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Pure_Function ([Entity =>] function_LOCAL_NAME);\n\nThis pragma appears in the same declarative part as a function\ndeclaration (or a set of function declarations if more than one\noverloaded declaration exists, in which case the pragma applies to all\nentities).  It specifies that the function `Entity' is to be considered\npure for the purposes of code generation.  This means that the compiler\ncan assume that there are no side effects, and in particular that two\ncalls with identical arguments produce the same result.  It also means\nthat the function can be used in an address clause.\n\nNote that, quite deliberately, there are no static checks to try to\nensure that this promise is met, so `Pure_Function' can be used with\nfunctions that are conceptually pure, even if they do modify global\nvariables.  For example, a square root function that is instrumented to\ncount the number of times it is called is still conceptually pure, and\ncan still be optimized, even though it modifies a global variable (the\ncount).  Memo functions are another example (where a table of previous\ncalls is kept and consulted to avoid re-computation).\n\nNote also that the normal rules excluding optimization of subprograms\nin pure units (when parameter types are descended from System.Address,\nor when the full view of a parameter type is limited), do not apply for\nthe Pure_Function case. If you explicitly specify Pure_Function, the\ncompiler may optimize away calls with identical arguments, and if that\nresults in unexpected behavior, the proper action is not to use the\npragma for subprograms that are not (conceptually) pure.\n\nNote: Most functions in a `Pure' package are automatically pure, and\nthere is no need to use pragma `Pure_Function' for such functions.  One\nexception is any function that has at least one formal of type\n`System.Address' or a type derived from it.  Such functions are not\nconsidered pure by default, since the compiler assumes that the\n`Address' parameter may be functioning as a pointer and that the\nreferenced data may change even if the address value does not.\nSimilarly, imported functions are not considered to be pure by default,\nsince there is no way of checking that they are in fact pure.  The use\nof pragma `Pure_Function' for such a function will override these\ndefault assumption, and cause the compiler to treat a designated\nsubprogram as pure in these cases.\n\nNote: If pragma `Pure_Function' is applied to a renamed function, it\napplies to the underlying renamed function.  This can be used to\ndisambiguate cases of overloading where some but not all functions in a\nset of overloaded functions are to be designated as pure.\n\nIf pragma `Pure_Function' is applied to a library-level function, the\nfunction is also considered pure from an optimization point of view,\nbut the unit is not a Pure unit in the categorization sense. So for\nexample, a function thus marked is free to `with' non-pure units.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Pure_Function""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Queuing_Policy (policy_identifier);""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Queuing_Policy""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Rational;\n\nThis pragma is considered obsolescent, but is retained for\ncompatibility purposes. It is equivalent to:\n\npragma Profile (Rational);""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Rational""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Ravenscar;\n\nThis pragma is considered obsolescent, but is retained for\ncompatibility purposes. It is equivalent to:\n\npragma Profile (Ravenscar);\n\nwhich is the preferred method of setting the `Ravenscar' profile.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Ravenscar""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Refined_Depends (DEPENDENCY_RELATION);\n\nDEPENDENCY_RELATION ::=\nnull\n| (DEPENDENCY_CLAUSE {, DEPENDENCY_CLAUSE})\n\nDEPENDENCY_CLAUSE ::=\nOUTPUT_LIST =>[+] INPUT_LIST\n| NULL_DEPENDENCY_CLAUSE\n\nNULL_DEPENDENCY_CLAUSE ::= null => INPUT_LIST\n\nOUTPUT_LIST ::= OUTPUT | (OUTPUT {, OUTPUT})\n\nINPUT_LIST ::= null | INPUT | (INPUT {, INPUT})\n\nOUTPUT ::= NAME | FUNCTION_RESULT\nINPUT  ::= NAME\n\nwhere FUNCTION_RESULT is a function Result attribute_reference\n\nFor the semantics of this pragma, see the entry for aspect\n`Refined_Depends' in the SPARK 2014 Reference Manual, section 6.1.5.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Refined_Depends""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Refined_Global (GLOBAL_SPECIFICATION);\n\nGLOBAL_SPECIFICATION ::=\nnull\n| (GLOBAL_LIST)\n| (MODED_GLOBAL_LIST {, MODED_GLOBAL_LIST})\n\nMODED_GLOBAL_LIST ::= MODE_SELECTOR => GLOBAL_LIST\n\nMODE_SELECTOR ::= In_Out | Input | Output | Proof_In\nGLOBAL_LIST   ::= GLOBAL_ITEM | (GLOBAL_ITEM {, GLOBAL_ITEM})\nGLOBAL_ITEM   ::= NAME\n\nFor the semantics of this pragma, see the entry for aspect\n`Refined_Global' in the SPARK 2014 Reference Manual, section 6.1.4.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Refined_Global""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Refined_Post (boolean_EXPRESSION);\n\nFor the semantics of this pragma, see the entry for aspect\n`Refined_Post' in the SPARK 2014 Reference Manual, section 7.2.7.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Refined_Post""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Refined_State (REFINEMENT_LIST);\n\nREFINEMENT_LIST ::=\n(REFINEMENT_CLAUSE {, REFINEMENT_CLAUSE})\n\nREFINEMENT_CLAUSE ::= state_NAME => CONSTITUENT_LIST\n\nCONSTITUENT_LIST ::=\nnull\n|  CONSTITUENT\n| (CONSTITUENT {, CONSTITUENT})\n\nCONSTITUENT ::= object_NAME | state_NAME\n\nFor the semantics of this pragma, see the entry for aspect\n`Refined_State' in the SPARK 2014 Reference Manual, section 7.2.2.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Refined_State""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Relative_Deadline (time_span_EXPRESSION);\n\nThis pragma is standard in Ada 2005, but is available in all earlier\nversions of Ada as an implementation-defined pragma.  See Ada 2012\nReference Manual for details.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Relative_Deadline""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Remote_Access_Type ([Entity =>] formal_access_type_LOCAL_NAME);\n\nThis pragma appears in the formal part of a generic declaration.  It\nspecifies an exception to the RM rule from E.2.2(17/2), which forbids\nthe use of a remote access to class-wide type as actual for a formal\naccess type.\n\nWhen this pragma applies to a formal access type `Entity', that type is\ntreated as a remote access to class-wide type in the generic.  It must\nbe a formal general access type, and its designated type must be the\nclass-wide type of a formal tagged limited private type from the same\ngeneric declaration.\n\nIn the generic unit, the formal type is subject to all restrictions\npertaining to remote access to class-wide types. At instantiation, the\nactual type must be a remote access to class-wide type.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Remote_Access_Type""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Remote_Call_Interface [(library_unit_name)];""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Remote_Call_Interface""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Remote_Types [(library_unit_name)];""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Remote_Types""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Rename_Pragma (\n[New_Name =>] IDENTIFIER,\n[Renamed  =>] pragma_IDENTIFIER);\n\nThis pragma provides a mechanism for supplying new names for existing\npragmas. The `New_Name' identifier can subsequently be used as a\nsynonym for the Renamed pragma. For example, suppose you have code that\nwas originally developed on a compiler that supports Inline_Only as an\nimplementation defined pragma. And suppose the semantics of pragma\nInline_Only are identical to (or at least very similar to) the GNAT\nimplementation defined pragma Inline_Always. You could globally replace\nInline_Only with Inline_Always.\n\nHowever, to avoid that source modification, you could instead add a\nconfiguration pragma:\n\npragma Rename_Pragma (\nNew_Name => Inline_Only,\nRenamed  => Inline_Always);\n\nThen GNAT will treat \""pragma Inline_Only ...\"" as if you had written\n\""pragma Inline_Always ...\"".\n\nPragma Inline_Only will not necessarily mean the same thing as the\nother Ada compiler; it's up to you to make sure the semantics are close\nenough.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Rename_Pragma""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Restricted_Run_Time;\n\nThis pragma is considered obsolescent, but is retained for\ncompatibility purposes. It is equivalent to:\n\npragma Profile (Restricted);\n\nwhich is the preferred method of setting the restricted run time\nprofile.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Restricted_Run_Time""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Restriction_Warnings\n(restriction_IDENTIFIER {, restriction_IDENTIFIER});\n\nThis pragma allows a series of restriction identifiers to be specified\n(the list of allowed identifiers is the same as for pragma\n`Restrictions'). For each of these identifiers the compiler checks for\nviolations of the restriction, but generates a warning message rather\nthan an error message if the restriction is violated.\n\nOne use of this is in situations where you want to know about\nviolations of a restriction, but you want to ignore some of these\nviolations. Consider this example, where you want to set Ada_95 mode\nand enable style checks, but you want to know about any other use of\nimplementation pragmas:\n\npragma Restriction_Warnings (No_Implementation_Pragmas);\npragma Warnings (Off, \""violation of No_Implementation_Pragmas\"");\npragma Ada_95;\npragma Style_Checks (\""2bfhkM160\"");\npragma Warnings (On, \""violation of No_Implementation_Pragmas\"");\n\nBy including the above lines in a configuration pragmas file, the\nAda_95 and Style_Checks pragmas are accepted without generating a\nwarning, but any other use of implementation defined pragmas will cause\na warning to be generated.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Restriction_Warnings""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Restrictions (restriction{, restriction});""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Restrictions""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Reviewable;\n\nThis pragma is an RM-defined standard pragma, but has no effect on the\nprogram being compiled, or on the code generated for the program.\n\nTo obtain the required output specified in RM H.3.1, the compiler must\nbe run with various special switches as follows:\n\n* `Where compiler-generated run-time checks remain'\n\nThe switch `-gnatGL' may be used to list the expanded code in\npseudo-Ada form.  Runtime checks show up in the listing either as\nexplicit checks or operators marked with {} to indicate a check is\npresent.\n\n* `An identification of known exceptions at compile time'\n\nIf the program is compiled with `-gnatwa', the compiler warning\nmessages will indicate all cases where the compiler detects that\nan exception is certain to occur at run time.\n\n* `Possible reads of uninitialized variables'\n\nThe compiler warns of many such cases, but its output is\nincomplete.\n\nThe CodePeer analysis tool may be used to obtain a comprehensive list\nof all possible points at which uninitialized data may be read.\n\n* `Where run-time support routines are implicitly invoked'\n\nIn the output from `-gnatGL', run-time calls are explicitly listed\nas calls to the relevant run-time routine.\n\n* `Object code listing'\n\nThis may be obtained either by using the `-S' switch, or the\nobjdump utility.\n\n* `Constructs known to be erroneous at compile time'\n\nThese are identified by warnings issued by the compiler (use\n`-gnatwa').\n\n* `Stack usage information'\n\nStatic stack usage data (maximum per-subprogram) can be obtained\nvia the `-fstack-usage' switch to the compiler.  Dynamic stack\nusage data (per task) can be obtained via the `-u' switch to\ngnatbind\n\nThe gnatstack utility can be used to provide additional information on\nstack usage.\n\n* `Object code listing of entire partition'\n\nThis can be obtained by compiling the partition with `-S', or by\napplying objdump to all the object files that are part of the\npartition.\n\n* `A description of the run-time model'\n\nThe full sources of the run-time are available, and the\ndocumentation of these routines describes how these run-time\nroutines interface to the underlying operating system facilities.\n\n* `Control and data-flow information'\n\nThe CodePeer tool may be used to obtain complete control and data-flow\ninformation, as well as comprehensive messages identifying possible\nproblems based on this information.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Reviewable""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma SPARK_Mode [(On | Off)] ;\n\nIn general a program can have some parts that are in SPARK 2014 (and\nfollow all the rules in the SPARK Reference Manual), and some parts\nthat are full Ada 2012.\n\nThe SPARK_Mode pragma is used to identify which parts are in SPARK 2014\n(by default programs are in full Ada). The SPARK_Mode pragma can be\nused in the following places:\n\n* As a configuration pragma, in which case it sets the default mode\nfor all units compiled with this pragma.\n\n* Immediately following a library-level subprogram spec\n\n* Immediately within a library-level package body\n\n* Immediately following the `private' keyword of a library-level\npackage spec\n\n* Immediately following the `begin' keyword of a library-level\npackage body\n\n* Immediately within a library-level subprogram body\n\nNormally a subprogram or package spec/body inherits the current mode\nthat is active at the point it is declared. But this can be overridden\nby pragma within the spec or body as above.\n\nThe basic consistency rule is that you can't turn SPARK_Mode back `On',\nonce you have explicitly (with a pragma) turned if `Off'. So the\nfollowing rules apply:\n\nIf a subprogram spec has SPARK_Mode `Off', then the body must also have\nSPARK_Mode `Off'.\n\nFor a package, we have four parts:\n\n* the package public declarations\n\n* the package private part\n\n* the body of the package\n\n* the elaboration code after `begin'\n\nFor a package, the rule is that if you explicitly turn SPARK_Mode `Off'\nfor any part, then all the following parts must have SPARK_Mode `Off'.\nNote that this may require repeating a pragma SPARK_Mode (`Off') in the\nbody. For example, if we have a configuration pragma SPARK_Mode (`On')\nthat turns the mode on by default everywhere, and one particular\npackage spec has pragma SPARK_Mode (`Off'), then that pragma will need\nto be repeated in the package body.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""SPARK_Mode""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Secondary_Stack_Size (integer_EXPRESSION);\n\nThis pragma appears within the task definition of a single task\ndeclaration or a task type declaration (like pragma `Storage_Size') and\napplies to all task objects of that type. The argument specifies the\nsize of the secondary stack to be used by these task objects, and must\nbe of an integer type. The secondary stack is used to handle functions\nthat return a variable-sized result, for example a function returning\nan unconstrained String.\n\nNote this pragma only applies to targets using fixed secondary stacks,\nlike VxWorks 653 and bare board targets, where a fixed block for the\nsecondary stack is allocated from the primary stack of the task. By\ndefault, these targets assign a percentage of the primary stack for the\nsecondary stack, as defined by `System.Parameter.Sec_Stack_Percentage'.\nWith this pragma, an `integer_EXPRESSION' of bytes is assigned from the\nprimary stack instead.\n\nFor most targets, the pragma does not apply as the secondary stack\ngrows on demand: allocated as a chain of blocks in the heap. The\ndefault size of these blocks can be modified via the `-D' binder option\nas described in `GNAT User's Guide'.\n\nNote that no check is made to see if the secondary stack can fit inside\nthe primary stack.\n\nNote the pragma cannot appear when the restriction `No_Secondary_Stack'\nis in effect.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Secondary_Stack_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Share_Generic (GNAME {, GNAME});\n\nGNAME ::= generic_unit_NAME | generic_instance_NAME\n\nThis pragma is provided for compatibility with Dec Ada 83. It has no\neffect in GNAT (which does not implement shared generics), other than\nto check that the given names are all names of generic units or generic\ninstances.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Share_Generic""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This pragma is provided for compatibility with Ada 83. The syntax and\nsemantics are identical to pragma Atomic.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Shared""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Shared_Passive [(library_unit_name)];""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Shared_Passive""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Short_Circuit_And_Or;\n\nThis configuration pragma causes any occurrence of the AND operator\napplied to operands of type Standard.Boolean to be short-circuited\n(i.e. the AND operator is treated as if it were AND THEN). Or is\nsimilarly treated as OR ELSE. This may be useful in the context of\ncertification protocols requiring the use of short-circuited logical\noperators. If this configuration pragma occurs locally within the file\nbeing compiled, it applies only to the file being compiled.  There is\nno requirement that all units in a partition use this option.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Short_Circuit_And_Or""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Short_Descriptors\n\nThis pragma is provided for compatibility with other Ada\nimplementations. It is recognized but ignored by all current versions\nof GNAT.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Short_Descriptors""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Simple_Storage_Pool_Type (type_LOCAL_NAME);\n\nA type can be established as a 'simple storage pool type' by applying\nthe representation pragma `Simple_Storage_Pool_Type' to the type.  A\ntype named in the pragma must be a library-level immutably limited\nrecord type or limited tagged type declared immediately within a\npackage declaration.  The type can also be a limited private type whose\nfull type is allowed as a simple storage pool type.\n\nFor a simple storage pool type `SSP', nonabstract primitive subprograms\n`Allocate', `Deallocate', and `Storage_Size' can be declared that are\nsubtype conformant with the following subprogram declarations:\n\nprocedure Allocate\n(Pool                     : in out SSP;\nStorage_Address          : out System.Address;\nSize_In_Storage_Elements : System.Storage_Elements.Storage_Count;\nAlignment                : System.Storage_Elements.Storage_Count);\n\nprocedure Deallocate\n(Pool : in out SSP;\nStorage_Address          : System.Address;\nSize_In_Storage_Elements : System.Storage_Elements.Storage_Count;\nAlignment                : System.Storage_Elements.Storage_Count);\n\nfunction Storage_Size (Pool : SSP)\nreturn System.Storage_Elements.Storage_Count;\n\nProcedure `Allocate' must be declared, whereas `Deallocate' and\n`Storage_Size' are optional. If `Deallocate' is not declared, then\napplying an unchecked deallocation has no effect other than to set its\nactual parameter to null. If `Storage_Size' is not declared, then the\n`Storage_Size' attribute applied to an access type associated with a\npool object of type SSP returns zero. Additional operations can be\ndeclared for a simple storage pool type (such as for supporting a\nmark/release storage-management discipline).\n\nAn object of a simple storage pool type can be associated with an access\ntype by specifying the attribute *note Simple_Storage_Pool: e9. For\nexample:\n\nMy_Pool : My_Simple_Storage_Pool_Type;\n\ntype Acc is access My_Data_Type;\n\nfor Acc'Simple_Storage_Pool use My_Pool;\n\nSee attribute *note Simple_Storage_Pool: e9.  for further details.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Simple_Storage_Pool_Type""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Source_File_Name (\n[Unit_Name   =>] unit_NAME,\nSpec_File_Name =>  STRING_LITERAL,\n[Index => INTEGER_LITERAL]);\n\npragma Source_File_Name (\n[Unit_Name   =>] unit_NAME,\nBody_File_Name =>  STRING_LITERAL,\n[Index => INTEGER_LITERAL]);\n\nUse this to override the normal naming convention.  It is a\nconfiguration pragma, and so has the usual applicability of\nconfiguration pragmas (i.e., it applies to either an entire partition,\nor to all units in a compilation, or to a single unit, depending on how\nit is used.  `unit_name' is mapped to `file_name_literal'.  The\nidentifier for the second argument is required, and indicates whether\nthis is the file name for the spec or for the body.\n\nThe optional Index argument should be used when a file contains multiple\nunits, and when you do not want to use `gnatchop' to separate then into\nmultiple files (which is the recommended procedure to limit the number\nof recompilations that are needed when some sources change).  For\ninstance, if the source file `source.ada' contains\n\npackage B is\n\nend B;\n\nwith B;\nprocedure A is\nbegin\n\nend A;\n\nyou could use the following configuration pragmas:\n\npragma Source_File_Name\n(B, Spec_File_Name => \""source.ada\"", Index => 1);\npragma Source_File_Name\n(A, Body_File_Name => \""source.ada\"", Index => 2);\n\nNote that the `gnatname' utility can also be used to generate those\nconfiguration pragmas.\n\nAnother form of the `Source_File_Name' pragma allows the specification\nof patterns defining alternative file naming schemes to apply to all\nfiles.\n\npragma Source_File_Name\n(  [Spec_File_Name  =>] STRING_LITERAL\n[,[Casing          =>] CASING_SPEC]\n[,[Dot_Replacement =>] STRING_LITERAL]);\n\npragma Source_File_Name\n(  [Body_File_Name  =>] STRING_LITERAL\n[,[Casing          =>] CASING_SPEC]\n[,[Dot_Replacement =>] STRING_LITERAL]);\n\npragma Source_File_Name\n(  [Subunit_File_Name =>] STRING_LITERAL\n[,[Casing            =>] CASING_SPEC]\n[,[Dot_Replacement   =>] STRING_LITERAL]);\n\nCASING_SPEC ::= Lowercase | Uppercase | Mixedcase\n\nThe first argument is a pattern that contains a single asterisk\nindicating the point at which the unit name is to be inserted in the\npattern string to form the file name.  The second argument is optional.\nIf present it specifies the casing of the unit name in the resulting\nfile name string.  The default is lower case.  Finally the third\nargument allows for systematic replacement of any dots in the unit name\nby the specified string literal.\n\nNote that Source_File_Name pragmas should not be used if you are using\nproject files. The reason for this rule is that the project manager is\nnot aware of these pragmas, and so other tools that use the projet file\nwould not be aware of the intended naming conventions. If you are using\nproject files, file naming is controlled by Source_File_Name_Project\npragmas, which are usually supplied automatically by the project\nmanager. A pragma Source_File_Name cannot appear after a *note Pragma\nSource_File_Name_Project: ec.\n\nFor more details on the use of the `Source_File_Name' pragma, see the\nsections on `Using Other File Names' and `Alternative File Naming\nSchemes' in the `GNAT User's Guide'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Source_File_Name""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""This pragma has the same syntax and semantics as pragma\nSource_File_Name.  It is only allowed as a stand-alone configuration\npragma.  It cannot appear after a *note Pragma Source_File_Name: ea, and\nmost importantly, once pragma Source_File_Name_Project appears, no\nfurther Source_File_Name pragmas are allowed.\n\nThe intention is that Source_File_Name_Project pragmas are always\ngenerated by the Project Manager in a manner consistent with the naming\nspecified in a project file, and when naming is controlled in this\nmanner, it is not permissible to attempt to modify this naming scheme\nusing Source_File_Name or Source_File_Name_Project pragmas (which would\nnot be known to the project manager).""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Source_File_Name_Project""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Source_Reference (INTEGER_LITERAL, STRING_LITERAL);\n\nThis pragma must appear as the first line of a source file.\n`integer_literal' is the logical line number of the line following the\npragma line (for use in error messages and debugging information).\n`string_literal' is a static string constant that specifies the file\nname to be used in error messages and debugging information.  This is\nmost notably used for the output of `gnatchop' with the `-r' switch, to\nmake sure that the original unchopped source file is the one referred\nto.\n\nThe second argument must be a string literal, it cannot be a static\nstring expression other than a string literal.  This is because its\nvalue is needed for error messages issued by all phases of the compiler.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Source_Reference""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Static_Elaboration_Desired;\n\nThis pragma is used to indicate that the compiler should attempt to\ninitialize statically the objects declared in the library unit to which\nthe pragma applies, when these objects are initialized (explicitly or\nimplicitly) by an aggregate.  In the absence of this pragma, aggregates\nin object declarations are expanded into assignments and loops, even\nwhen the aggregate components are static constants. When the aggregate\nis present the compiler builds a static expression that requires no\nrun-time code, so that the initialized object can be placed in\nread-only data space. If the components are not static, or the\naggregate has more that 100 components, the compiler emits a warning\nthat the pragma cannot be obeyed. (See also the restriction\nNo_Implicit_Loops, which supports static construction of larger\naggregates with static components that include an others choice.)""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Static_Elaboration_Desired""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Storage_Size;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Storage_Size""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Stream_Convert (\n[Entity =>] type_LOCAL_NAME,\n[Read   =>] function_NAME,\n[Write  =>] function_NAME);\n\nThis pragma provides an efficient way of providing user-defined stream\nattributes.  Not only is it simpler to use than specifying the\nattributes directly, but more importantly, it allows the specification\nto be made in such a way that the predefined unit Ada.Streams is not\nloaded unless it is actually needed (i.e. unless the stream attributes\nare actually used); the use of the Stream_Convert pragma adds no\noverhead at all, unless the stream attributes are actually used on the\ndesignated type.\n\nThe first argument specifies the type for which stream functions are\nprovided.  The second parameter provides a function used to read values\nof this type.  It must name a function whose argument type may be any\nsubtype, and whose returned type must be the type given as the first\nargument to the pragma.\n\nThe meaning of the `Read' parameter is that if a stream attribute\ndirectly or indirectly specifies reading of the type given as the first\nparameter, then a value of the type given as the argument to the Read\nfunction is read from the stream, and then the Read function is used to\nconvert this to the required target type.\n\nSimilarly the `Write' parameter specifies how to treat write attributes\nthat directly or indirectly apply to the type given as the first\nparameter.  It must have an input parameter of the type specified by\nthe first parameter, and the return type must be the same as the input\ntype of the Read function.  The effect is to first call the Write\nfunction to convert to the given stream type, and then write the result\ntype to the stream.\n\nThe Read and Write functions must not be overloaded subprograms.  If\nnecessary renamings can be supplied to meet this requirement.  The\nusage of this attribute is best illustrated by a simple example, taken\nfrom the GNAT implementation of package Ada.Strings.Unbounded:\n\nfunction To_Unbounded (S : String) return Unbounded_String\nrenames To_Unbounded_String;\n\npragma Stream_Convert\n(Unbounded_String, To_Unbounded, To_String);\n\nThe specifications of the referenced functions, as given in the Ada\nReference Manual are:\n\nfunction To_Unbounded_String (Source : String)\nreturn Unbounded_String;\n\nfunction To_String (Source : Unbounded_String)\nreturn String;\n\nThe effect is that if the value of an unbounded string is written to a\nstream, then the representation of the item in the stream is in the\nsame format that would be used for `Standard.String'Output', and this\nsame representation is expected when a value of this type is read from\nthe stream. Note that the value written always includes the bounds,\neven for Unbounded_String'Write, since Unbounded_String is not an array\ntype.\n\nNote that the `Stream_Convert' pragma is not effective in the case of a\nderived type of a non-limited tagged type. If such a type is specified\nthen the pragma is silently ignored, and the default implementation of\nthe stream attributes is used instead.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Stream_Convert""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Style_Checks (string_LITERAL | ALL_CHECKS |\nOn | Off [, LOCAL_NAME]);\n\nThis pragma is used in conjunction with compiler switches to control the\nbuilt in style checking provided by GNAT.  The compiler switches, if\nset, provide an initial setting for the switches, and this pragma may\nbe used to modify these settings, or the settings may be provided\nentirely by the use of the pragma.  This pragma can be used anywhere\nthat a pragma is legal, including use as a configuration pragma\n(including use in the `gnat.adc' file).\n\nThe form with a string literal specifies which style options are to be\nactivated.  These are additive, so they apply in addition to any\npreviously set style check options.  The codes for the options are the\nsame as those used in the `-gnaty' switch to `gcc' or `gnatmake'.  For\nexample the following two methods can be used to enable layout checking:\n\n*     pragma Style_Checks (\""l\"");\n\n*     gcc -c -gnatyl ...\n\nThe form `ALL_CHECKS' activates all standard checks (its use is\nequivalent to the use of the `gnaty' switch with no options.  See the\n`GNAT User's Guide' for details.)\n\nNote: the behavior is slightly different in GNAT mode (`-gnatg' used).\nIn this case, `ALL_CHECKS' implies the standard set of GNAT mode style\ncheck options (i.e. equivalent to `-gnatyg').\n\nThe forms with `Off' and `On' can be used to temporarily disable style\nchecks as shown in the following example:\n\npragma Style_Checks (\""k\""); -- requires keywords in lower case\npragma Style_Checks (Off); -- turn off style checks\nNULL;                      -- this will not generate an error message\npragma Style_Checks (On);  -- turn style checks back on\nNULL;                      -- this will generate an error message\n\nFinally the two argument form is allowed only if the first argument is\n`On' or `Off'.  The effect is to turn of semantic style checks for the\nspecified entity, as shown in the following example:\n\npragma Style_Checks (\""r\""); -- require consistency of identifier casing\nArg : Integer;\nRf1 : Integer := ARG;      -- incorrect, wrong case\npragma Style_Checks (Off, Arg);\nRf2 : Integer := ARG;      -- OK, no error""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Style_Checks""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Subtitle ([Subtitle =>] STRING_LITERAL);\n\nThis pragma is recognized for compatibility with other Ada compilers\nbut is ignored by GNAT.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Subtitle""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Suppress (Identifier [, [On =>] Name]);\n\nThis is a standard pragma, and supports all the check names required in\nthe RM. It is included here because GNAT recognizes some additional\ncheck names that are implementation defined (as permitted by the RM):\n\n* `Alignment_Check' can be used to suppress alignment checks on\naddresses used in address clauses. Such checks can also be\nsuppressed by suppressing range checks, but the specific use of\n`Alignment_Check' allows suppression of alignment checks without\nsuppressing other range checks.  Note that `Alignment_Check' is\nsuppressed by default on machines (such as the x86) with\nnon-strict alignment.\n\n* `Atomic_Synchronization' can be used to suppress the special memory\nsynchronization instructions that are normally generated for\naccess to `Atomic' variables to ensure correct synchronization\nbetween tasks that use such variables for synchronization purposes.\n\n* `Duplicated_Tag_Check' Can be used to suppress the check that is\ngenerated for a duplicated tag value when a tagged type is\ndeclared.\n\n* `Container_Checks' Can be used to suppress all checks within\nAda.Containers and instances of its children, including\nTampering_Check.\n\n* `Tampering_Check' Can be used to suppress tampering check in the\ncontainers.\n\n* `Predicate_Check' can be used to control whether predicate checks\nare active. It is applicable only to predicates for which the\npolicy is `Check'. Unlike `Assertion_Policy', which determines if\na given predicate is ignored or checked for the whole program, the\nuse of `Suppress' and `Unsuppress' with this check name allows a\ngiven predicate to be turned on and off at specific points in the\nprogram.\n\n* `Validity_Check' can be used specifically to control validity\nchecks.  If `Suppress' is used to suppress validity checks, then\nno validity checks are performed, including those specified by the\nappropriate compiler switch or the `Validity_Checks' pragma.\n\n* Additional check names previously introduced by use of the\n`Check_Name' pragma are also allowed.\n\nNote that pragma Suppress gives the compiler permission to omit checks,\nbut does not require the compiler to omit checks. The compiler will\ngenerate checks if they are essentially free, even when they are\nsuppressed. In particular, if the compiler can prove that a certain\ncheck will necessarily fail, it will generate code to do an\nunconditional 'raise', even if checks are suppressed. The compiler\nwarns in this case.\n\nOf course, run-time checks are omitted whenever the compiler can prove\nthat they will not fail, whether or not checks are suppressed.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Suppress""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Suppress_All;\n\nThis pragma can appear anywhere within a unit.  The effect is to apply\n`Suppress (All_Checks)' to the unit in which it appears.  This pragma\nis implemented for compatibility with DEC Ada 83 usage where it appears\nat the end of a unit, and for compatibility with Rational Ada, where it\nappears as a program unit pragma.  The use of the standard Ada pragma\n`Suppress (All_Checks)' as a normal configuration pragma is the\npreferred usage in GNAT.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Suppress_All""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Suppress_Debug_Info ([Entity =>] LOCAL_NAME);\n\nThis pragma can be used to suppress generation of debug information for\nthe specified entity. It is intended primarily for use in debugging the\ndebugger, and navigating around debugger problems.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Suppress_Debug_Info""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Suppress_Exception_Locations;\n\nIn normal mode, a raise statement for an exception by default generates\nan exception message giving the file name and line number for the\nlocation of the raise. This is useful for debugging and logging\npurposes, but this entails extra space for the strings for the\nmessages. The configuration pragma `Suppress_Exception_Locations' can\nbe used to suppress the generation of these strings, with the result\nthat space is saved, but the exception message for such raises is null.\nThis configuration pragma may appear in a global configuration pragma\nfile, or in a specific unit as usual. It is not required that this\npragma be used consistently within a partition, so it is fine to have\nsome units within a partition compiled with this pragma and others\ncompiled in normal mode without it.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Suppress_Exception_Locations""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Suppress_Initialization ([Entity =>] variable_or_subtype_Name);\n\nHere variable_or_subtype_Name is the name introduced by a type\ndeclaration or subtype declaration or the name of a variable introduced\nby an object declaration.\n\nIn the case of a type or subtype this pragma suppresses any implicit or\nexplicit initialization for all variables of the given type or subtype,\nincluding initialization resulting from the use of pragmas\nNormalize_Scalars or Initialize_Scalars.\n\nThis is considered a representation item, so it cannot be given after\nthe type is frozen. It applies to all subsequent object declarations,\nand also any allocator that creates objects of the type.\n\nIf the pragma is given for the first subtype, then it is considered to\napply to the base type and all its subtypes. If the pragma is given for\nother than a first subtype, then it applies only to the given subtype.\nThe pragma may not be given after the type is frozen.\n\nNote that this includes eliminating initialization of discriminants for\ndiscriminated types, and tags for tagged types. In these cases, you\nwill have to use some non-portable mechanism (e.g. address overlays or\nunchecked conversion) to achieve required initialization of these\nfields before accessing any object of the corresponding type.\n\nFor the variable case, implicit initialization for the named variable\nis suppressed, just as though its subtype had been given in a pragma\nSuppress_Initialization, as described above.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Suppress_Initialization""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Task_Dispatching_Policy (policy_identifier);""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Task_Dispatching_Policy""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax\n\npragma Task_Name (string_EXPRESSION);\n\nThis pragma appears within a task definition (like pragma `Priority')\nand applies to the task in which it appears.  The argument must be of\ntype String, and provides a name to be used for the task instance when\nthe task is created.  Note that this expression is not required to be\nstatic, and in particular, it can contain references to task\ndiscriminants.  This facility can be used to provide different names\nfor different tasks as they are created, as illustrated in the example\nbelow.\n\nThe task name is recorded internally in the run-time structures and is\naccessible to tools like the debugger.  In addition the routine\n`Ada.Task_Identification.Image' will return this string, with a unique\ntask address appended.\n\n--  Example of the use of pragma Task_Name\n\nwith Ada.Task_Identification;\nuse Ada.Task_Identification;\nwith Text_IO; use Text_IO;\nprocedure t3 is\n\ntype Astring is access String;\n\ntask type Task_Typ (Name : access String) is\npragma Task_Name (Name.all);\nend Task_Typ;\n\ntask body Task_Typ is\nNam : constant String := Image (Current_Task);\nbegin\nPut_Line (\""-->\"" & Nam (1 .. 14) & \""<--\"");\nend Task_Typ;\n\ntype Ptr_Task is access Task_Typ;\nTask_Var : Ptr_Task;\n\nbegin\nTask_Var :=\nnew Task_Typ (new String'(\""This is task 1\""));\nTask_Var :=\nnew Task_Typ (new String'(\""This is task 2\""));\nend;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Task_Name""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Task_Storage (\n[Task_Type =>] LOCAL_NAME,\n[Top_Guard =>] static_integer_EXPRESSION);\n\nThis pragma specifies the length of the guard area for tasks.  The guard\narea is an additional storage area allocated to a task.  A value of zero\nmeans that either no guard area is created or a minimal guard area is\ncreated, depending on the target.  This pragma can appear anywhere a\n`Storage_Size' attribute definition clause is allowed for a task type.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Task_Storage""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Test_Case (\n[Name     =>] static_string_Expression\n,[Mode     =>] (Nominal | Robustness)\n[, Requires =>  Boolean_Expression]\n[, Ensures  =>  Boolean_Expression]);\n\nThe `Test_Case' pragma allows defining fine-grain specifications for\nuse by testing tools.  The compiler checks the validity of the\n`Test_Case' pragma, but its presence does not lead to any modification\nof the code generated by the compiler.\n\n`Test_Case' pragmas may only appear immediately following the\n(separate) declaration of a subprogram in a package declaration, inside\na package spec unit. Only other pragmas may intervene (that is appear\nbetween the subprogram declaration and a test case).\n\nThe compiler checks that boolean expressions given in `Requires' and\n`Ensures' are valid, where the rules for `Requires' are the same as the\nrule for an expression in `Precondition' and the rules for `Ensures'\nare the same as the rule for an expression in `Postcondition'. In\nparticular, attributes `'Old' and `'Result' can only be used within the\n`Ensures' expression. The following is an example of use within a\npackage spec:\n\npackage Math_Functions is\n\nfunction Sqrt (Arg : Float) return Float;\npragma Test_Case (Name     => \""Test 1\"",\nMode     => Nominal,\nRequires => Arg < 10000,\nEnsures  => Sqrt'Result < 10);\n\nend Math_Functions;\n\nThe meaning of a test case is that there is at least one context where\n`Requires' holds such that, if the associated subprogram is executed in\nthat context, then `Ensures' holds when the subprogram returns.  Mode\n`Nominal' indicates that the input context should also satisfy the\nprecondition of the subprogram, and the output context should also\nsatisfy its postcondition. Mode `Robustness' indicates that the\nprecondition and postcondition of the subprogram should be ignored for\nthis test case.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Test_Case""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Thread_Local_Storage ([Entity =>] LOCAL_NAME);\n\nThis pragma specifies that the specified entity, which must be a\nvariable declared in a library-level package, is to be marked as\n\""Thread Local Storage\"" (`TLS'). On systems supporting this (which\ninclude Windows, Solaris, GNU/Linux, and VxWorks 6), this causes each\nthread (and hence each Ada task) to see a distinct copy of the variable.\n\nThe variable must not have default initialization, and if there is an\nexplicit initialization, it must be either `null' for an access\nvariable, a static expression for a scalar variable, or a fully static\naggregate for a composite type, that is to say, an aggregate all of\nwhose components are static, and which does not include packed or\ndiscriminated components.\n\nThis provides a low-level mechanism similar to that provided by the\n`Ada.Task_Attributes' package, but much more efficient and is also\nuseful in writing interface code that will interact with foreign\nthreads.\n\nIf this pragma is used on a system where `TLS' is not supported, then\nan error message will be generated and the program will be rejected.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Thread_Local_Storage""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Time_Slice (static_duration_EXPRESSION);\n\nFor implementations of GNAT on operating systems where it is possible\nto supply a time slice value, this pragma may be used for this purpose.\nIt is ignored if it is used in a system that does not allow this\ncontrol, or if it appears in other than the main program unit.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Time_Slice""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Title (TITLING_OPTION [, TITLING OPTION]);\n\nTITLING_OPTION ::=\n[Title    =>] STRING_LITERAL,\n| [Subtitle =>] STRING_LITERAL\n\nSyntax checked but otherwise ignored by GNAT.  This is a listing control\npragma used in DEC Ada 83 implementations to provide a title and/or\nsubtitle for the program listing.  The program listing generated by GNAT\ndoes not have titles or subtitles.\n\nUnlike other pragmas, the full flexibility of named notation is allowed\nfor this pragma, i.e., the parameters may be given in any order if named\nnotation is used, and named and positional notation can be mixed\nfollowing the normal rules for procedure calls in Ada.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Title""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Type_Invariant\n([Entity =>] type_LOCAL_NAME,\n[Check  =>] EXPRESSION);\n\nThe `Type_Invariant' pragma is intended to be an exact replacement for\nthe language-defined `Type_Invariant' aspect, and shares its\nrestrictions and semantics. It differs from the language defined\n`Invariant' pragma in that it does not permit a string parameter, and\nit is controlled by the assertion identifier `Type_Invariant' rather\nthan `Invariant'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Type_Invariant""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Type_Invariant_Class\n([Entity =>] type_LOCAL_NAME,\n[Check  =>] EXPRESSION);\n\nThe `Type_Invariant_Class' pragma is intended to be an exact\nreplacement for the language-defined `Type_Invariant'Class' aspect, and\nshares its restrictions and semantics.\n\nNote: This pragma is called `Type_Invariant_Class' rather than\n`Type_Invariant'Class' because the latter would not be strictly\nconforming to the allowed syntax for pragmas. The motivation for\nproviding pragmas equivalent to the aspects is to allow a program to be\nwritten using the pragmas, and then compiled if necessary using an Ada\ncompiler that does not recognize the pragmas or aspects, but is\nprepared to ignore the pragmas. The assertion policy that controls this\npragma is `Type_Invariant'Class', not `Type_Invariant_Class'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Type_Invariant_Class""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Unchecked_Union (first_subtype_LOCAL_NAME);\n\nThis pragma is used to specify a representation of a record type that is\nequivalent to a C union. It was introduced as a GNAT implementation\ndefined pragma in the GNAT Ada 95 mode. Ada 2005 includes an extended\nversion of this pragma, making it language defined, and GNAT fully\nimplements this extended version in all language modes (Ada 83, Ada 95,\nand Ada 2005). For full details, consult the Ada 2012 Reference Manual,\nsection B.3.3.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unchecked_Union""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Unevaluated_Use_Of_Old (Error | Warn | Allow);\n\nThis pragma controls the processing of attributes Old and Loop_Entry.\nIf either of these attributes is used in a potentially unevaluated\nexpression  (e.g. the then or else parts of an if expression), then\nnormally this usage is considered illegal if the prefix of the attribute\nis other than an entity name. The language requires this behavior for\nOld, and GNAT copies the same rule for Loop_Entry.\n\nThe reason for this rule is that otherwise, we can have a situation\nwhere we save the Old value, and this results in an exception, even\nthough we might not evaluate the attribute. Consider this example:\n\npackage UnevalOld is\nK : Character;\nprocedure U (A : String; C : Boolean)  -- ERROR\nwith Post => (if C then A(1)'Old = K else True);\nend;\n\nIf procedure U is called with a string with a lower bound of 2, and C\nfalse, then an exception would be raised trying to evaluate A(1) on\nentry even though the value would not be actually used.\n\nAlthough the rule guarantees against this possibility, it is sometimes\ntoo restrictive. For example if we know that the string has a lower\nbound of 1, then we will never raise an exception.  The pragma\n`Unevaluated_Use_Of_Old' can be used to modify this behavior. If the\nargument is `Error' then an error is given (this is the default RM\nbehavior). If the argument is `Warn' then the usage is allowed as legal\nbut with a warning that an exception might be raised. If the argument\nis `Allow' then the usage is allowed as legal without generating a\nwarning.\n\nThis pragma may appear as a configuration pragma, or in a declarative\npart or package specification. In the latter case it applies to uses up\nto the end of the corresponding statement sequence or sequence of\npackage declarations.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unevaluated_Use_Of_Old""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Unimplemented_Unit;\n\nIf this pragma occurs in a unit that is processed by the compiler, GNAT\naborts with the message `xxx not implemented', where `xxx' is the name\nof the current compilation unit.  This pragma is intended to allow the\ncompiler to handle unimplemented library units in a clean manner.\n\nThe abort only happens if code is being generated.  Thus you can use\nspecs of unimplemented packages in syntax or semantic checking mode.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unimplemented_Unit""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Universal_Aliasing [([Entity =>] type_LOCAL_NAME)];\n\n`type_LOCAL_NAME' must refer to a type declaration in the current\ndeclarative part.  The effect is to inhibit strict type-based aliasing\noptimization for the given type.  In other words, the effect is as\nthough access types designating this type were subject to pragma\nNo_Strict_Aliasing.  For a detailed description of the strict aliasing\noptimization, and the situations in which it must be suppressed, see\nthe section on `Optimization and Strict Aliasing' in the `GNAT User's\nGuide'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Universal_Aliasing""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Universal_Data [(library_unit_Name)];\n\nThis pragma is supported only for the AAMP target and is ignored for\nother targets. The pragma specifies that all library-level objects\n(Counter 0 data) associated with the library unit are to be accessed\nand updated using universal addressing (24-bit addresses for AAMP5)\nrather than the default of 16-bit Data Environment (DENV) addressing.\nUse of this pragma will generally result in less efficient code for\nreferences to global data associated with the library unit, but allows\nsuch data to be located anywhere in memory. This pragma is a library\nunit pragma, but can also be used as a configuration pragma (including\nuse in the `gnat.adc' file). The functionality of this pragma is also\navailable by applying the -univ switch on the compilations of units\nwhere universal addressing of the data is desired.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Universal_Data""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Unmodified (LOCAL_NAME {, LOCAL_NAME});\n\nThis pragma signals that the assignable entities (variables, `out'\nparameters, `in out' parameters) whose names are listed are\ndeliberately not assigned in the current source unit. This suppresses\nwarnings about the entities being referenced but not assigned, and in\naddition a warning will be generated if one of these entities is in\nfact assigned in the same unit as the pragma (or in the corresponding\nbody, or one of its subunits).\n\nThis is particularly useful for clearly signaling that a particular\nparameter is not modified, even though the spec suggests that it might\nbe.\n\nFor the variable case, warnings are never given for unreferenced\nvariables whose name contains one of the substrings `DISCARD, DUMMY,\nIGNORE, JUNK, UNUSED' in any casing. Such names are typically to be\nused in cases where such warnings are expected.  Thus it is never\nnecessary to use `pragma Unmodified' for such variables, though it is\nharmless to do so.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unmodified""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Unreferenced (LOCAL_NAME {, LOCAL_NAME});\npragma Unreferenced (library_unit_NAME {, library_unit_NAME});\n\nThis pragma signals that the entities whose names are listed are\ndeliberately not referenced in the current source unit after the\noccurrence of the pragma. This suppresses warnings about the entities\nbeing unreferenced, and in addition a warning will be generated if one\nof these entities is in fact subsequently referenced in the same unit\nas the pragma (or in the corresponding body, or one of its subunits).\n\nThis is particularly useful for clearly signaling that a particular\nparameter is not referenced in some particular subprogram implementation\nand that this is deliberate. It can also be useful in the case of\nobjects declared only for their initialization or finalization side\neffects.\n\nIf `LOCAL_NAME' identifies more than one matching homonym in the\ncurrent scope, then the entity most recently declared is the one to\nwhich the pragma applies. Note that in the case of accept formals, the\npragma Unreferenced may appear immediately after the keyword `do' which\nallows the indication of whether or not accept formals are referenced\nor not to be given individually for each accept statement.\n\nThe left hand side of an assignment does not count as a reference for\nthe purpose of this pragma. Thus it is fine to assign to an entity for\nwhich pragma Unreferenced is given.\n\nNote that if a warning is desired for all calls to a given subprogram,\nregardless of whether they occur in the same unit as the subprogram\ndeclaration, then this pragma should not be used (calls from another\nunit would not be flagged); pragma Obsolescent can be used instead for\nthis purpose, see *note Pragma Obsolescent: af.\n\nThe second form of pragma `Unreferenced' is used within a context\nclause. In this case the arguments must be unit names of units\npreviously mentioned in `with' clauses (similar to the usage of pragma\n`Elaborate_All'. The effect is to suppress warnings about unreferenced\nunits and unreferenced entities within these units.\n\nFor the variable case, warnings are never given for unreferenced\nvariables whose name contains one of the substrings `DISCARD, DUMMY,\nIGNORE, JUNK, UNUSED' in any casing. Such names are typically to be\nused in cases where such warnings are expected.  Thus it is never\nnecessary to use `pragma Unreferenced' for such variables, though it is\nharmless to do so.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unreferenced""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Unreferenced_Objects (local_subtype_NAME {, local_subtype_NAME});\n\nThis pragma signals that for the types or subtypes whose names are\nlisted, objects which are declared with one of these types or subtypes\nmay not be referenced, and if no references appear, no warnings are\ngiven.\n\nThis is particularly useful for objects which are declared solely for\ntheir initialization and finalization effect. Such variables are\nsometimes referred to as RAII variables (Resource Acquisition Is\nInitialization). Using this pragma on the relevant type (most typically\na limited controlled type), the compiler will automatically suppress\nunwanted warnings about these variables not being referenced.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unreferenced_Objects""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Unreserve_All_Interrupts;\n\nNormally certain interrupts are reserved to the implementation.  Any\nattempt to attach an interrupt causes Program_Error to be raised, as\ndescribed in RM C.3.2(22).  A typical example is the `SIGINT' interrupt\nused in many systems for a `Ctrl-C' interrupt.  Normally this interrupt\nis reserved to the implementation, so that `Ctrl-C' can be used to\ninterrupt execution.\n\nIf the pragma `Unreserve_All_Interrupts' appears anywhere in any unit in\na program, then all such interrupts are unreserved.  This allows the\nprogram to handle these interrupts, but disables their standard\nfunctions.  For example, if this pragma is used, then pressing `Ctrl-C'\nwill not automatically interrupt execution.  However, a program can\nthen handle the `SIGINT' interrupt as it chooses.\n\nFor a full list of the interrupts handled in a specific implementation,\nsee the source code for the spec of `Ada.Interrupts.Names' in file\n`a-intnam.ads'.  This is a target dependent file that contains the list\nof interrupts recognized for a given target.  The documentation in this\nfile also specifies what interrupts are affected by the use of the\n`Unreserve_All_Interrupts' pragma.\n\nFor a more general facility for controlling what interrupts can be\nhandled, see pragma `Interrupt_State', which subsumes the functionality\nof the `Unreserve_All_Interrupts' pragma.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unreserve_All_Interrupts""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Unsuppress (IDENTIFIER [, [On =>] NAME]);\n\nThis pragma undoes the effect of a previous pragma `Suppress'.  If\nthere is no corresponding pragma `Suppress' in effect, it has no\neffect.  The range of the effect is the same as for pragma `Suppress'.\nThe meaning of the arguments is identical to that used in pragma\n`Suppress'.\n\nOne important application is to ensure that checks are on in cases where\ncode depends on the checks for its correct functioning, so that the code\nwill compile correctly even if the compiler switches are set to suppress\nchecks. For example, in a program that depends on external names of\ntagged types and wants to ensure that the duplicated tag check occurs\neven if all run-time checks are suppressed by a compiler switch, the\nfollowing configuration pragma will ensure this test is not suppressed:\n\npragma Unsuppress (Duplicated_Tag_Check);\n\nThis pragma is standard in Ada 2005. It is available in all earlier\nversions of Ada as an implementation-defined pragma.\n\nNote that in addition to the checks defined in the Ada RM, GNAT\nrecogizes a number of implementation-defined check names. See the\ndescription of pragma `Suppress' for full details.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unsuppress""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Unused (LOCAL_NAME {, LOCAL_NAME});\n\nThis pragma signals that the assignable entities (variables, `out'\nparameters, and `in out' parameters) whose names are listed\ndeliberately do not get assigned or referenced in the current source\nunit after the occurrence of the pragma in the current source unit. This\nsuppresses warnings about the entities that are unreferenced and/or not\nassigned, and, in addition, a warning will be generated if one of these\nentities gets assigned or subsequently referenced in the same unit as\nthe pragma (in the corresponding body or one of its subunits).\n\nThis is particularly useful for clearly signaling that a particular\nparameter is not modified or referenced, even though the spec suggests\nthat it might be.\n\nFor the variable case, warnings are never given for unreferenced\nvariables whose name contains one of the substrings `DISCARD, DUMMY,\nIGNORE, JUNK, UNUSED' in any casing. Such names are typically to be\nused in cases where such warnings are expected.  Thus it is never\nnecessary to use `pragma Unmodified' for such variables, though it is\nharmless to do so.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Unused""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Use_VADS_Size;\n\nThis is a configuration pragma.  In a unit to which it applies, any use\nof the 'Size attribute is automatically interpreted as a use of the\n'VADS_Size attribute.  Note that this may result in incorrect semantic\nprocessing of valid Ada 95 or Ada 2005 programs.  This is intended to\naid in the handling of existing code which depends on the\ninterpretation of Size as implemented in the VADS compiler.  See\ndescription of the VADS_Size attribute for further details.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Use_VADS_Size""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Validity_Checks (string_LITERAL | ALL_CHECKS | On | Off);\n\nThis pragma is used in conjunction with compiler switches to control the\nbuilt-in validity checking provided by GNAT.  The compiler switches, if\nset provide an initial setting for the switches, and this pragma may be\nused to modify these settings, or the settings may be provided entirely\nby the use of the pragma.  This pragma can be used anywhere that a\npragma is legal, including use as a configuration pragma (including use\nin the `gnat.adc' file).\n\nThe form with a string literal specifies which validity options are to\nbe activated.  The validity checks are first set to include only the\ndefault reference manual settings, and then a string of letters in the\nstring specifies the exact set of options required.  The form of this\nstring is exactly as described for the `-gnatVx' compiler switch (see\nthe GNAT User's Guide for details).  For example the following two\nmethods can be used to enable validity checking for mode `in' and `in\nout' subprogram parameters:\n\n*     pragma Validity_Checks (\""im\"");\n\n*     $ gcc -c -gnatVim ...\n\nThe form ALL_CHECKS activates all standard checks (its use is equivalent\nto the use of the `gnatVa' switch).\n\nThe forms with `Off' and `On' can be used to temporarily disable\nvalidity checks as shown in the following example:\n\npragma Validity_Checks (\""c\""); -- validity checks for copies\npragma Validity_Checks (Off); -- turn off validity checks\nA := B;                       -- B will not be validity checked\npragma Validity_Checks (On);  -- turn validity checks back on\nA := C;                       -- C will be validity checked""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Validity_Checks""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Volatile (LOCAL_NAME);\n\nThis pragma is defined by the Ada Reference Manual, and the GNAT\nimplementation is fully conformant with this definition.  The reason it\nis mentioned in this section is that a pragma of the same name was\nsupplied in some Ada 83 compilers, including DEC Ada 83.  The Ada 95 /\nAda 2005 implementation of pragma Volatile is upwards compatible with\nthe implementation in DEC Ada 83.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Volatile""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Volatile_Components;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Volatile_Components""," & ASCII.LF
   & """_origin"": ""Ada RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Volatile_Full_Access (LOCAL_NAME);\n\nThis is similar in effect to pragma Volatile, except that any reference\nto the object is guaranteed to be done only with instructions that read\nor write all the bits of the object. Furthermore, if the object is of a\ncomposite type, then any reference to a component of the object is\nguaranteed to read and/or write all the bits of the object.\n\nThe intention is that this be suitable for use with memory-mapped I/O\ndevices on some machines. Note that there are two important respects in\nwhich this is different from `pragma Atomic'. First a reference to a\n`Volatile_Full_Access' object is not a sequential action in the RM 9.10\nsense and, therefore, does not create a synchronization point. Second,\nin the case of `pragma Atomic', there is no guarantee that all the bits\nwill be accessed if the reference is not to the whole object; the\ncompiler is allowed (and generally will) access only part of the object\nin this case.\n\nIt is not permissible to specify `Atomic' and `Volatile_Full_Access' for\nthe same object.\n\nIt is not permissible to specify `Volatile_Full_Access' for a composite\n(record or array) type or object that has at least one `Aliased'\ncomponent.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Volatile_Full_Access""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Volatile_Function [ (boolean_EXPRESSION) ];\n\nFor the semantics of this pragma, see the entry for aspect\n`Volatile_Function' in the SPARK 2014 Reference Manual, section 7.1.2.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Volatile_Function""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Warning_As_Error (static_string_EXPRESSION);\n\nThis configuration pragma allows the programmer to specify a set of\nwarnings that will be treated as errors. Any warning that matches the\npattern given by the pragma argument will be treated as an error. This\ngives more precise control than -gnatwe, which treats warnings as\nerrors.\n\nThis pragma can apply to regular warnings (messages enabled by -gnatw)\nand to style warnings (messages that start with \""(style)\"", enabled by\n-gnaty).\n\nThe pattern may contain asterisks, which match zero or more characters\nin the message. For example, you can use `pragma Warning_As_Error\n(\""bits of*unused\"")' to treat the warning message `warning: 960 bits of\n\""a\"" unused' as an error. All characters other than asterisk are treated\nas literal characters in the match. The match is case insensitive; for\nexample XYZ matches xyz.\n\nNote that the pattern matches if it occurs anywhere within the warning\nmessage string (it is not necessary to put an asterisk at the start and\nthe end of the message, since this is implied).\n\nAnother possibility for the static_string_EXPRESSION which works whether\nor not error tags are enabled (`-gnatw.d') is to use a single `-gnatw'\ntag string, enclosed in brackets, as shown in the example below, to\ntreat one category of warnings as errors.  Note that if you want to\ntreat multiple categories of warnings as errors, you can use multiple\npragma Warning_As_Error.\n\nThe above use of patterns to match the message applies only to warning\nmessages generated by the front end. This pragma can also be applied to\nwarnings provided by the back end and mentioned in *note Pragma\nWarnings: 121.  By using a single full `-Wxxx' switch in the pragma,\nsuch warnings can also be treated as errors.\n\nThe pragma can appear either in a global configuration pragma file\n(e.g. `gnat.adc'), or at the start of a file. Given a global\nconfiguration pragma file containing:\n\npragma Warning_As_Error (\""[-gnatwj]\"");\n\nwhich will treat all obsolescent feature warnings as errors, the\nfollowing program compiles as shown (compile options here are\n`-gnatwa.d -gnatl -gnatj55').\n\npragma Warning_As_Error (\""*never assigned*\"");\nfunction Warnerr return String is\nX : Integer;\n|\n>>> error: variable \""X\"" is never read and\nnever assigned [-gnatwv] [warning-as-error]\n\nY : Integer;\n|\n>>> warning: variable \""Y\"" is assigned but\nnever read [-gnatwu]\n\nbegin\nY := 0;\nreturn %ABC%;\n|\n>>> error: use of \""%\"" is an obsolescent\nfeature (RM J.2(4)), use \""\""\"" instead\n[-gnatwj] [warning-as-error]\n\nend;\n\nlines: No errors, 3 warnings (2 treated as errors)\n\nNote that this pragma does not affect the set of warnings issued in any\nway, it merely changes the effect of a matching warning if one is\nproduced as a result of other warnings options. As shown in this\nexample, if the pragma results in a warning being treated as an error,\nthe tag is changed from \""warning:\"" to \""error:\"" and the string\n\""[warning-as-error]\"" is appended to the end of the message.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Warning_As_Error""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Warnings ([TOOL_NAME,] DETAILS [, REASON]);\n\nDETAILS ::= On | Off\nDETAILS ::= On | Off, local_NAME\nDETAILS ::= static_string_EXPRESSION\nDETAILS ::= On | Off, static_string_EXPRESSION\n\nTOOL_NAME ::= GNAT | GNATProve\n\nREASON ::= Reason => STRING_LITERAL {& STRING_LITERAL}\n\nNote: in Ada 83 mode, a string literal may be used in place of a static\nstring expression (which does not exist in Ada 83).\n\nNote if the second argument of `DETAILS' is a `local_NAME' then the\nsecond form is always understood. If the intention is to use the fourth\nform, then you can write `NAME & \""\""' to force the intepretation as a\n`static_string_EXPRESSION'.\n\nNote: if the first argument is a valid `TOOL_NAME', it will be\ninterpreted that way. The use of the `TOOL_NAME' argument is relevant\nonly to users of SPARK and GNATprove, see last part of this section for\ndetails.\n\nNormally warnings are enabled, with the output being controlled by the\ncommand line switch.  Warnings (`Off') turns off generation of warnings\nuntil a Warnings (`On') is encountered or the end of the current unit.\nIf generation of warnings is turned off using this pragma, then some or\nall of the warning messages are suppressed, regardless of the setting\nof the command line switches.\n\nThe `Reason' parameter may optionally appear as the last argument in\nany of the forms of this pragma. It is intended purely for the purposes\nof documenting the reason for the `Warnings' pragma.  The compiler will\ncheck that the argument is a static string but otherwise ignore this\nargument. Other tools may provide specialized processing for this\nstring.\n\nThe form with a single argument (or two arguments if Reason present),\nwhere the first argument is `ON' or `OFF' may be used as a\nconfiguration pragma.\n\nIf the `LOCAL_NAME' parameter is present, warnings are suppressed for\nthe specified entity.  This suppression is effective from the point\nwhere it occurs till the end of the extended scope of the variable\n(similar to the scope of `Suppress'). This form cannot be used as a\nconfiguration pragma.\n\nIn the case where the first argument is other than `ON' or `OFF', the\nthird form with a single static_string_EXPRESSION argument (and possible\nreason) provides more precise control over which warnings are active.\nThe string is a list of letters specifying which warnings are to be\nactivated and which deactivated. The code for these letters is the same\nas the string used in the command line switch controlling warnings. For\na brief summary, use the gnatmake command with no arguments, which will\ngenerate usage information containing the list of warnings switches\nsupported. For full details see the section on `Warning Message\nControl' in the `GNAT User's Guide'.  This form can also be used as a\nconfiguration pragma.\n\nThe warnings controlled by the `-gnatw' switch are generated by the\nfront end of the compiler. The GCC back end can provide additional\nwarnings and they are controlled by the `-W' switch. Such warnings can\nbe identified by the appearance of a string of the form `[-W{xxx}]' in\nthe message which designates the `-W`xxx'' switch that controls the\nmessage.  The form with a single `static_string_EXPRESSION' argument\nalso works for these warnings, but the string must be a single full\n`-W`xxx'' switch in this case. The above reference lists a few examples\nof these additional warnings.\n\nThe specified warnings will be in effect until the end of the program\nor another pragma `Warnings' is encountered. The effect of the pragma is\ncumulative. Initially the set of warnings is the standard default set\nas possibly modified by compiler switches. Then each pragma Warning\nmodifies this set of warnings as specified. This form of the pragma may\nalso be used as a configuration pragma.\n\nThe fourth form, with an `On|Off' parameter and a string, is used to\ncontrol individual messages, based on their text. The string argument\nis a pattern that is used to match against the text of individual\nwarning messages (not including the initial \""warning: \"" tag).\n\nThe pattern may contain asterisks, which match zero or more characters\nin the message. For example, you can use `pragma Warnings (Off, \""bits\nof*unused\"")' to suppress the warning message `warning: 960 bits of \""a\""\nunused'. No other regular expression notations are permitted. All\ncharacters other than asterisk in these three specific cases are\ntreated as literal characters in the match.  The match is case\ninsensitive, for example XYZ matches xyz.\n\nNote that the pattern matches if it occurs anywhere within the warning\nmessage string (it is not necessary to put an asterisk at the start and\nthe end of the message, since this is implied).\n\nThe above use of patterns to match the message applies only to warning\nmessages generated by the front end. This form of the pragma with a\nstring argument can also be used to control warnings provided by the\nback end and mentioned above. By using a single full `-W`xxx'' switch\nin the pragma, such warnings can be turned on and off.\n\nThere are two ways to use the pragma in this form. The OFF form can be\nused as a configuration pragma. The effect is to suppress all warnings\n(if any) that match the pattern string throughout the compilation (or\nmatch the -W switch in the back end case).\n\nThe second usage is to suppress a warning locally, and in this case, two\npragmas must appear in sequence:\n\npragma Warnings (Off, Pattern);\ncode where given warning is to be suppressed\npragma Warnings (On, Pattern);\n\nIn this usage, the pattern string must match in the Off and On pragmas,\nand (if `-gnatw.w' is given) at least one matching warning must be\nsuppressed.\n\nNote: if the ON form is not found, then the effect of the OFF form\nextends until the end of the file (pragma Warnings is purely textual,\nso its effect does not stop at the end of the enclosing scope).\n\nNote: to write a string that will match any warning, use the string\n`\""***\""'. It will not work to use a single asterisk or two asterisks\nsince this looks like an operator name. This form with three asterisks\nis similar in effect to specifying `pragma Warnings (Off)' except (if\n`-gnatw.w' is given) that a matching `pragma Warnings (On, \""***\"")' will\nbe required. This can be helpful in avoiding forgetting to turn\nwarnings back on.\n\nNote: the debug flag `-gnatd.i' can be used to cause the compiler to\nentirely ignore all WARNINGS pragmas. This can be useful in checking\nwhether obsolete pragmas in existing programs are hiding real problems.\n\nNote: pragma Warnings does not affect the processing of style messages.\nSee separate entry for pragma Style_Checks for control of style\nmessages.\n\nUsers of the formal verification tool GNATprove for the SPARK subset of\nAda may use the version of the pragma with a `TOOL_NAME' parameter.\n\nIf present, `TOOL_NAME' is the name of a tool, currently either `GNAT'\nfor the compiler or `GNATprove' for the formal verification tool. A\ngiven tool only takes into account pragma Warnings that do not specify\na tool name, or that specify the matching tool name. This makes it\npossible to disable warnings selectively for each tool, and as a\nconsequence to detect useless pragma Warnings with switch `-gnatw.w'.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Warnings""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Weak_External ([Entity =>] LOCAL_NAME);\n\n`LOCAL_NAME' must refer to an object that is declared at the library\nlevel. This pragma specifies that the given entity should be marked as a\nweak symbol for the linker. It is equivalent to `__attribute__((weak))'\nin GNU C and causes `LOCAL_NAME' to be emitted as a weak symbol instead\nof a regular symbol, that is to say a symbol that does not have to be\nresolved by the linker if used in conjunction with a pragma Import.\n\nWhen a weak symbol is not resolved by the linker, its address is set to\nzero. This is useful in writing interfaces to external modules that may\nor may not be linked in the final executable, for example depending on\nconfiguration settings.\n\nIf a program references at run time an entity to which this pragma has\nbeen applied, and the corresponding symbol was not resolved at link\ntime, then the execution of the program is erroneous. It is not\nerroneous to take the Address of such an entity, for example to guard\npotential references, as shown in the example below.\n\nSome file formats do not support weak symbols so not all target machines\nsupport this pragma.\n\n--  Example of the use of pragma Weak_External\n\npackage External_Module is\nkey : Integer;\npragma Import (C, key);\npragma Weak_External (key);\nfunction Present return boolean;\nend External_Module;\n\nwith System; use System;\npackage body External_Module is\nfunction Present return boolean is\nbegin\nreturn key'Address /= System.Null_Address;\nend Present;\nend External_Module;""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Weak_External""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """DOC"": ""Syntax:\n\npragma Wide_Character_Encoding (IDENTIFIER | CHARACTER_LITERAL);\n\nThis pragma specifies the wide character encoding to be used in program\nsource text appearing subsequently. It is a configuration pragma, but\nmay also be used at any point that a pragma is allowed, and it is\npermissible to have more than one such pragma in a file, allowing\nmultiple encodings to appear within the same file.\n\nHowever, note that the pragma cannot immediately precede the relevant\nwide character, because then the previous encoding will still be in\neffect, causing \""illegal character\"" errors.\n\nThe argument can be an identifier or a character literal. In the\nidentifier case, it is one of `HEX', `UPPER', `SHIFT_JIS', `EUC',\n`UTF8', or `BRACKETS'. In the character literal case it is\ncorrespondingly one of the characters `h', `u', `s', `e', `8', or `b'.\n\nNote that when the pragma is used within a file, it affects only the\nencoding within that file, and does not affect withed units, specs, or\nsubunits.""," & ASCII.LF
   & """_id"": ""0""," & ASCII.LF
   & """_name"": ""Wide_Character_Encoding""," & ASCII.LF
   & """_origin"": ""GNAT RM""" & ASCII.LF
   & "}" & ASCII.LF
   & "]," & ASCII.LF
   & """STANDARD"": [" & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""&\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\"">\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\"">=\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""<\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""<=\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""*\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""**\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""+\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""-\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""/\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""/=\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""=\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""abs\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""and\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""mod\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""not\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""or\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""rem\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""\""xor\""""," & ASCII.LF
   & """_category"": ""function""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """FIELD"": [" & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""NUL""," & ASCII.LF
   & """_doc"": ""16#00#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""SOH""," & ASCII.LF
   & """_doc"": ""16#01#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""STX""," & ASCII.LF
   & """_doc"": ""16#02#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""ETX""," & ASCII.LF
   & """_doc"": ""16#03#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""EOT""," & ASCII.LF
   & """_doc"": ""16#04#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""ENQ""," & ASCII.LF
   & """_doc"": ""16#05#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""ACK""," & ASCII.LF
   & """_doc"": ""16#06#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""BEL""," & ASCII.LF
   & """_doc"": ""16#07#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""BS""," & ASCII.LF
   & """_doc"": ""16#08#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""HT""," & ASCII.LF
   & """_doc"": ""16#09#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LF""," & ASCII.LF
   & """_doc"": ""16#0A#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""VT""," & ASCII.LF
   & """_doc"": ""16#0B#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""FF""," & ASCII.LF
   & """_doc"": ""16#0C#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""CR""," & ASCII.LF
   & """_doc"": ""16#0D#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""SO""," & ASCII.LF
   & """_doc"": ""16#0E#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""SI""," & ASCII.LF
   & """_doc"": ""16#0F#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""DLE""," & ASCII.LF
   & """_doc"": ""16#10#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""DC1""," & ASCII.LF
   & """_doc"": ""16#11#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""DC2""," & ASCII.LF
   & """_doc"": ""16#12#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""DC3""," & ASCII.LF
   & """_doc"": ""16#13#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""DC4""," & ASCII.LF
   & """_doc"": ""16#14#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""NAK""," & ASCII.LF
   & """_doc"": ""16#15#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""SYN""," & ASCII.LF
   & """_doc"": ""16#16#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""ETB""," & ASCII.LF
   & """_doc"": ""16#17#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""CAN""," & ASCII.LF
   & """_doc"": ""16#18#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""EM""," & ASCII.LF
   & """_doc"": ""16#19#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""SUB""," & ASCII.LF
   & """_doc"": ""16#1A#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""ESC""," & ASCII.LF
   & """_doc"": ""16#1B#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""FS""," & ASCII.LF
   & """_doc"": ""16#1C#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""GS""," & ASCII.LF
   & """_doc"": ""16#1D#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""RS""," & ASCII.LF
   & """_doc"": ""16#1E#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""US""," & ASCII.LF
   & """_doc"": ""16#1F#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Exclam""," & ASCII.LF
   & """_doc"": ""16#21#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Quotation""," & ASCII.LF
   & """_doc"": ""16#22#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Sharp""," & ASCII.LF
   & """_doc"": ""16#23#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Dollar""," & ASCII.LF
   & """_doc"": ""16#24#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Percent""," & ASCII.LF
   & """_doc"": ""16#25#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Ampersand""," & ASCII.LF
   & """_doc"": ""16#26#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Colon""," & ASCII.LF
   & """_doc"": ""16#3A#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Semicolon""," & ASCII.LF
   & """_doc"": ""16#3B#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Query""," & ASCII.LF
   & """_doc"": ""16#3F#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""At_Sign""," & ASCII.LF
   & """_doc"": ""16#40#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""L_Bracket""," & ASCII.LF
   & """_doc"": ""16#5B#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Back_Slash""," & ASCII.LF
   & """_doc"": ""16#5C#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""R_Bracket""," & ASCII.LF
   & """_doc"": ""16#5D#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Circumflex""," & ASCII.LF
   & """_doc"": ""16#5E#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Underline""," & ASCII.LF
   & """_doc"": ""16#5F#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Grave""," & ASCII.LF
   & """_doc"": ""16#60#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_A""," & ASCII.LF
   & """_doc"": ""16#61#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_B""," & ASCII.LF
   & """_doc"": ""16#62#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_C""," & ASCII.LF
   & """_doc"": ""16#63#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_D""," & ASCII.LF
   & """_doc"": ""16#64#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_E""," & ASCII.LF
   & """_doc"": ""16#65#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_F""," & ASCII.LF
   & """_doc"": ""16#66#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_G""," & ASCII.LF
   & """_doc"": ""16#67#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_H""," & ASCII.LF
   & """_doc"": ""16#68#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_I""," & ASCII.LF
   & """_doc"": ""16#69#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_J""," & ASCII.LF
   & """_doc"": ""16#6A#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_K""," & ASCII.LF
   & """_doc"": ""16#6B#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_L""," & ASCII.LF
   & """_doc"": ""16#6C#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_M""," & ASCII.LF
   & """_doc"": ""16#6D#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_N""," & ASCII.LF
   & """_doc"": ""16#6E#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_O""," & ASCII.LF
   & """_doc"": ""16#6F#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_P""," & ASCII.LF
   & """_doc"": ""16#70#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_Q""," & ASCII.LF
   & """_doc"": ""16#71#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_R""," & ASCII.LF
   & """_doc"": ""16#72#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_S""," & ASCII.LF
   & """_doc"": ""16#73#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_T""," & ASCII.LF
   & """_doc"": ""16#74#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_U""," & ASCII.LF
   & """_doc"": ""16#75#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_V""," & ASCII.LF
   & """_doc"": ""16#76#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_W""," & ASCII.LF
   & """_doc"": ""16#77#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_X""," & ASCII.LF
   & """_doc"": ""16#78#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_Y""," & ASCII.LF
   & """_doc"": ""16#79#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""LC_Z""," & ASCII.LF
   & """_doc"": ""16#7A#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""L_BRACE""," & ASCII.LF
   & """_doc"": ""16#7B#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""BA""," & ASCII.LF
   & """_doc"": ""16#7C#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""R_BRACE""," & ASCII.LF
   & """_doc"": ""16#7D#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""TILDE""," & ASCII.LF
   & """_doc"": ""16#7E#""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""DEL""," & ASCII.LF
   & """_doc"": ""16#7F#""" & ASCII.LF
   & "}" & ASCII.LF
   & "]," & ASCII.LF
   & """_name"": ""ASCII""," & ASCII.LF
   & """_category"": ""package""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Boolean""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Character""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Constraint_Error""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Duration""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""False""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Float""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Integer""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Long_Float""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Long_Integer""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Long_Long_Float""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Long_Long_Integer""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Natural""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Numeric_Error""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Positive""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Program_Error""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Short_Float""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Short_Integer""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Short_Short_Integer""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Storage_Error""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""String""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Tasking_Error""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""True""," & ASCII.LF
   & """_category"": ""variable""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Wide_Character""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Wide_String""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Wide_Wide_Character""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}," & ASCII.LF
   & "{" & ASCII.LF
   & """_name"": ""Wide_Wide_String""," & ASCII.LF
   & """_category"": ""type""" & ASCII.LF
   & "}" & ASCII.LF
   & "]" & ASCII.LF
   & "}" & ASCII.LF
   & "}" & ASCII.LF;

end LSP.Predefined_Completion.Ada2012;
