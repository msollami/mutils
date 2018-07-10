BeginTestSection["tests"]

VerificationTest[(* 1 *)
	DropColumn[List[1, 2, 3], 2]
	,
	$Failed	
	,
	TestID->"fa2dd618-a37b-4b9a-9b2e-219d85121cda"
]

VerificationTest[(* 2 *)
	DropColumn[List[List[1, 2, 3]], 2]
	,
	List[List[1, 3]]	
	,
	TestID->"b802f82f-2ada-48bc-b9f6-6676407c2f13"
]

EndTestSection[]
