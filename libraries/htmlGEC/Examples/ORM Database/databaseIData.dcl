definition module databaseIData

import StdEnv, StdMaybe
import StdHtml

import databaseDef, support

	
// specially defined and generated iData

derive gForm 	[], Maybe,
				Academic, X_DepartmentName, Phone, Access_Level, Contract, University, X_Degree, Rank, Chair,
				Appointment, Teacher, Professor, TeachingAndProf, Person, Room, Building,
				Uniq, Opt, Others, Topic, X_TopicCode, Degrees, Topics,
				Department, MyDatabase, RefAcademicus, Academics, Departments,RefDept

derive gUpd 	[], Maybe,
				Academic, X_DepartmentName, Phone, Access_Level, Contract, University, X_Degree, Rank, Chair,
				Appointment, Teacher, Professor, TeachingAndProf, Person, Room, Building,
				Uniq, Opt, Others, Topic, X_TopicCode, Degrees, Topics,
				Department, MyDatabase, RefAcademicus, Academics, Departments,RefDept

derive gPrint 	Maybe,
				Academic, X_DepartmentName, Phone, Access_Level, Contract, University, X_Degree, Rank, Chair,
				Appointment, Teacher, Professor, TeachingAndProf, Person, Room, Building,
				Uniq, Opt, Others, Topic, X_TopicCode, Degrees, Topics,
				Department, MyDatabase, RefAcademicus, Academics, Departments,RefDept

derive gParse 	Maybe,
				Academic, X_DepartmentName, Phone, Access_Level, Contract, University, X_Degree, Rank, Chair,
				Appointment, Teacher, Professor, TeachingAndProf, Person, Room, Building,
				Uniq, Opt, Others, Topic, X_TopicCode, Degrees, Topics,
				Department, MyDatabase, RefAcademicus, Academics, Departments,RefDept

	
	
	