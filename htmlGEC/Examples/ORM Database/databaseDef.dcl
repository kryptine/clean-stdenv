definition module databaseDef

import StdMaybe, support

// Mydatabase

:: MyDatabase  	=	Academics	Academics
				|	Departments	Departments
				|	Empty

:: Academics	=	Academic 	[Academic]		// Shared Table
:: Departments	=	Department 	[Department]	// Shared Table

// types with a special effect

:: Uniq 		=	Uniq Int		// will automatically generate non editable unique number

// Academic types

:: Academic		= 	{ nr 			:: Uniq
					, person		:: Person
					, works_for		:: RefDept
					, room			:: Room
					, phone			:: Phone
					, contract		:: Contract
					, rank			:: Rank
					, degree		:: Degrees 
					, appointment	:: Appointment
					, teaches		:: Topics
					}
:: Person		= 	{ surname		:: String
					, name			:: String
					, phone			:: Opt Int
					}
:: Opt a		=	Yes a | No
:: RefDept		= 	RefDept Int
:: Room			=	Room RoomNr Building
:: RoomNr		:== Int
:: Building		=	ScienceBuilding
				|	AcademicHospital		
:: Phone		=	{ extension		:: Int
					, access_Level	:: Access_Level
					}	
:: Access_Level	=	International
				|	National
				| 	Local
:: Contract		=	Tenjured
				|	Till HtmlDate
:: University	=	RUU
				|	TUE
				|	UT
				|	RU
:: Rank			=	Prof
				|	SeniorLecturer
				|	Lecturer
:: Degrees		=	Degree [(X_Degree,University)]
:: X_Degree		=	DSc
				|	MSc
				|	BSc
:: Appointment =	TeachingAndProf TeachingAndProf
				|	Professor		Professor	
				|	Teacher 		Teacher
				|	Standard
:: Teacher		=	{ audited	:: Others
				}
:: Others		=	Other [RefTeachers]
:: RefTeachers	:== Int
:: Professor	=	{ chair 	:: Chair
					, heads		:: Opt RefDept
					}
:: Chair		=	Foundation
				|	Information_Systems
				|	Software_Technology
:: TeachingAndProf 
				=	{ serves	:: String
					, teacher	:: Teacher
					, professor :: Professor
					}
:: Topics		=	Topic [Topic]
:: Topic		=	{ topic 	:: X_TopicCode 
					, rating	:: Opt Rating
					}
:: Rating		:== Int
:: X_TopicCode	=	ProgramI
				|	ProgramII
				|	FoundationI
				|	FuncProgramm
					
// Department types

:: Department	=	{	dnr 			:: Uniq
					,	departmentName	:: X_DepartmentName
					,	teachingBudget	:: Dollars
					,	researchBudget	:: Dollars
					,	head			:: RefAcademicus
					}
:: X_DepartmentName
				=	BioChemistry  
				| 	Mathematics
				| 	Philosopy
				| 	ComputerScience
				|	NoDepartment
:: Dollars		:==	Int
:: RefAcademicus
				=	RefAcad Int


	
	
	