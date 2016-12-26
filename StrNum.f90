	!模块StrNum.F90的代码如下:

	!------------------------------------------------------------
	!---StrNum.F90:提供字符串,数字和文本文件处理的功能子程序
	!---吴徐平2013-07-22(wxp07@qq.com)
	!------------------------------------------------------------
	module StrNum
	!----------------------------------------------
	implicit none
	!----------------------------------------------
	!---字符串转换为数字数StrToNum(InStr,Num,Error)
	interface StrToNum
	module procedure StrToReal4
	module procedure StrToReal8
	module procedure StrToInteger1
	module procedure StrToInteger2
	module procedure StrToInteger4
	end interface
	!----------------------------------------------
	!---保留InStr中的浮点数相关的字符,其它字符全部变为空格KeepRealChar(InStr)
	interface KeepRealChar
	module procedure KeepRealChar
	end interface
	!----------------------------------------------
	!---保留InStr中的浮点数相关的字符,其它字符全部变为空格KeepNumChar(InStr)
	interface KeepNumChar
	module procedure KeepNumChar
	end interface
	!----------------------------------------------
	!---识别InStr中左右有效可见字符(33-126)的索引TrimIndex(InStr,LeftIndex,RightIndex,Error)
	interface TrimIndex
	module procedure TrimIndex
	end interface
	!----------------------------------------------
	!---字符串分割StringSplit(InStr,delimiter,StrArray,nsize)
	interface StringSplit
	module procedure StringSplit
	end interface
	!----------------------------------------------
	!---字符串替换StrReplace(InStr,OldChar,NewChar,OutStr)
	interface StrReplace
	module procedure StrReplace
	end interface
	!----------------------------------------------
	!---字符串变为浮点数组StrToRealArray(InStr,RealArray,nsize)
	interface StrToRealArray
	module procedure StrToRealArray4
	module procedure StrToRealArray8
	end interface
	!----------------------------------------------
	!---测试字符串是否可以转为RealArray数组:IsRealArrayString(InStr,Error)
	interface IsRealArrayString
	module procedure IsRealArrayString
	end interface
	!----------------------------------------------
	!---获取文本文件FileName行列信息:GetFileRowColumn(FileName,HeadLine,Row,Column)
	interface GetFileRowColumn
	module procedure GetFileRowColumn
	end interface
	!----------------------------------------------
	!---文件数据的读取LoadFromFile(FileName,Array2D,Row,Column,Error)
	interface LoadFromFile
	module procedure LoadFromFile4
	module procedure LoadFromFile8
	end interface
	!----------------------------------------------
	!**********************************************************
	contains
	!**********************************************************
	!=============================================================
	subroutine StrToReal4(InStr,Num,Error)
	!------------------------------------------------------------
	!---将字符串InStr转为Num数字类型
	!---如果Error == 0 ::表示InStr可以转为Num,否则转换错误
	!---吴徐平2013-07-20(wxp07@qq.com)
	!------------------------------------------------------------
	Implicit None
	Character(Len = *), Intent( IN ) :: InStr
	Character(Len = LEN(InStr)):: Str_temp
	Real(kind = 4),Intent( INOUT ) :: Num
	Integer,Intent( INOUT ) :: Error
	Integer::LeftIndex,RightIndex
	!-----------------
	Num=0
	Error=0
	!-----------------
	if (LEN(TRIM(InStr))>0 ) then
		Str_temp=InStr !为了不修改原始字符串的内容
		call KeepRealChar(Str_temp) !只保留浮点数相关的字符
		call TrimIndex(Str_temp,LeftIndex,RightIndex,Error)
		!-----------------
		if (Error==0 ) then
			Read( Str_temp(LeftIndex:RightIndex) , * ,iostat=Error) Num
		else
			Error=Error+1
		end if
	else
		Error=Error+1
	end if
	!-----------------
	end subroutine StrToReal4
	!
	!=============================================================
	subroutine StrToReal8(InStr,Num,Error)
	!------------------------------------------------------------
	!---将字符串InStr转为Num数字类型
	!---如果Error == 0 ::表示InStr可以转为Num,否则转换错误
	!---吴徐平2013-07-20(wxp07@qq.com)
	!------------------------------------------------------------
	Implicit None
	Character(Len = *), Intent( IN ) :: InStr
	Character(Len = LEN(InStr)):: Str_temp
	Real(kind = 8),Intent( INOUT ) :: Num
	Integer,Intent( INOUT ) :: Error
	Integer::LeftIndex,RightIndex
	!-----------------
	Num=0
	Error=0
	!-----------------
	if (LEN(TRIM(InStr))>0 ) then
		Str_temp=InStr !为了不修改原始字符串的内容
		call KeepRealChar(Str_temp) !只保留浮点数相关的字符
		call TrimIndex(Str_temp,LeftIndex,RightIndex,Error)
		!-----------------
		if (Error==0 ) then
			Read( Str_temp(LeftIndex:RightIndex) , * ,iostat=Error) Num
		else
			Error=Error+1
		end if
		!-----------------
	else
		Error=Error+1
	end if
	end subroutine StrToReal8
	!
	!=============================================================
	!=============================================================
	subroutine StrToInteger1(InStr,Num,Error)
	!------------------------------------------------------------
	!---将字符串InStr转为Num数字类型
	!---如果Error == 0 ::表示InStr可以转为Num,否则转换错误
	!---吴徐平2013-07-20(wxp07@qq.com)
	!------------------------------------------------------------
	Implicit None
	Character(Len = *), Intent( IN ) :: InStr
	Character(Len = LEN(InStr)):: Str_temp
	Integer(kind = 1),Intent( INOUT ) :: Num
	Integer,Intent( INOUT ) :: Error
	Integer::LeftIndex,RightIndex
	!-----------------
	Num=0
	Error=0
	!-----------------
	if (LEN(TRIM(InStr))>0 ) then
		Str_temp=InStr !为了不修改原始字符串的内容
		call KeepRealChar(Str_temp) !只保留浮点数相关的字符
		call TrimIndex(Str_temp,LeftIndex,RightIndex,Error)
		!-----------------
		if (Error==0 ) then
			Read( Str_temp(LeftIndex:RightIndex) , * ,iostat=Error) Num
		else
			Error=Error+1
		end if
		!-----------------
	else
		Error=Error+1
	end if
	end subroutine StrToInteger1
	!
	!=============================================================
	subroutine StrToInteger2(InStr,Num,Error)
	!------------------------------------------------------------
	!---将字符串InStr转为Num数字类型
	!---如果Error == 0 ::表示InStr可以转为Num,否则转换错误
	!---吴徐平2013-07-20(wxp07@qq.com)
	!------------------------------------------------------------
	Implicit None
	Character(Len = *), Intent( IN ) :: InStr
	Character(Len = LEN(InStr)):: Str_temp
	Integer(kind = 2),Intent( INOUT ) :: Num
	Integer,Intent( INOUT ) :: Error
	Integer::LeftIndex,RightIndex
	!-----------------
	Num=0
	Error=0
	!-----------------
	if (LEN(TRIM(InStr))>0 ) then
		Str_temp=InStr !为了不修改原始字符串的内容
		call KeepRealChar(Str_temp) !只保留浮点数相关的字符
		call TrimIndex(Str_temp,LeftIndex,RightIndex,Error)
		!-----------------
		if (Error==0 ) then
			Read( Str_temp(LeftIndex:RightIndex) , * ,iostat=Error) Num
		else
			Error=Error+1
		end if
		!-----------------
	else
		Error=Error+1
	end if

	!-----------------
	end subroutine StrToInteger2
	!
	!=============================================================
	subroutine StrToInteger4(InStr,Num,Error)
	!------------------------------------------------------------
	!---将字符串InStr转为Num数字类型
	!---如果Error == 0 ::表示InStr可以转为Num,否则转换错误
	!---吴徐平2013-07-20(wxp07@qq.com)
	!------------------------------------------------------------
	Implicit None
	Character(Len = *), Intent( IN ) :: InStr
	Character(Len = LEN(InStr)):: Str_temp
	Integer(kind = 4),Intent( INOUT ) :: Num
	Integer,Intent( INOUT ) :: Error
	Integer::LeftIndex,RightIndex
	!-----------------
	Num=0
	Error=0
	!-----------------
	if (LEN(TRIM(InStr))>0 ) then
		!-----------------
		Str_temp=InStr !为了不修改原始字符串的内容
		call KeepRealChar(Str_temp) !只保留浮点数相关的字符
		call TrimIndex(Str_temp,LeftIndex,RightIndex,Error)
		!-----------------
		if (Error==0 ) then
			Read( Str_temp(LeftIndex:RightIndex) , * ,iostat=Error) Num
		else
			Error=Error+1
		end if
		!-----------------
	else
		Error=Error+1
	end if
	end subroutine StrToInteger4
	!
	!=============================================================
	subroutine KeepRealChar(InStr)
	!------------------------------------------------------------
	!---保留InStr中的浮点数相关的字符,其它字符全部变为空格
	!---吴徐平2013-07-20(wxp07@qq.com)
	!------------------------------------------------------------
	Implicit None
	Character(Len =*),Intent( INOUT ) :: InStr
	Character(Len =17):: RealChar='+-.0123456789eEdD'
	Character(Len =4):: StartChar='eEdD'
	Character(Len =6):: EndChar='eEdD+-'
	Character(Len =7):: SingleChar='eEdD+-.'
	!------------------------------------------------------------
	Integer ::i,j,k,flag,Error
	!------------------------------------------------------------
	do i=1,LEN(InStr)
		flag=0
		!-------------------------------
		do j=1,LEN(RealChar)
			!-------------------------------
			if (InStr(i:i)==RealChar(j:j)) then
				flag=flag+1  !-识别为RealChar浮点数字符
				Exit
			end if
			!------------------------------
		end do
		!-------------------------------
		if (flag==0) then
			InStr(i:i)=' '  !-非RealChar浮点数字符,置为空格
		end if
		!------------------------------
	end do
	!------------------------------------------------------------
	!---第一个有效字符不能为StartChar='eEdD'
	do while(.TRUE.)
		call TrimIndex(InStr,i,j,Error)
		if (Error==0)then
			!-------------------------------
			flag=0
			!------------------------------
			do k=1,LEN(StartChar)
				!-------------------------------
				if (InStr(i:i)==StartChar(k:k)) then
					flag=flag+1  !-第一个有效字符不能为eEdD
					Exit
				end if
				!------------------------------
			end do
			!------------------------------
			if (flag>0)then
				InStr(i:i)=' ' !将该字符置为空格
			else
				EXIT	!-第一个有效字符不是eEdD
			end if
			!------------------------------
		else
			EXIT
		end if
	end do
	!------------------------------------------------------------
	!---最后一个有效字符不能为EndChar='eEdD+-'
	do while(.TRUE.)
		call TrimIndex(InStr,i,j,Error)
		if (Error==0)then
			!-------------------------------
			flag=0
			!------------------------------
			do k=1,LEN(EndChar)
				!-------------------------------
				if (InStr(j:j)==EndChar(k:k)) then
					flag=flag+1  !-最后一个有效字符不能为EndChar='eEdD+-'
					Exit
				end if
				!------------------------------
			end do
			!------------------------------
			if (flag>0)then
				InStr(j:j)=' ' !将该字符置为空格
			else
				EXIT	!-最后一个有效字符不是EndChar='eEdD+-'
			end if
			!------------------------------
		else
			EXIT
		end if
	end do
	!------------------------------------------------------------
	!---如果只含有一个有效字符,则不能是SingleChar='eEdD+-.'
	do while(.TRUE.)
		call TrimIndex(InStr,i,j,Error)
		if ((Error==0) .AND. (i==j))then
			!-------------------------------
			flag=0
			!------------------------------
			do k=1,LEN(SingleChar)
				!-------------------------------
				if (InStr(i:i)==SingleChar(k:k)) then
					flag=flag+1  !-有效字符不能为SingleChar
					Exit
				end if
				!------------------------------
			end do
			!------------------------------
			if (flag>0)then
				InStr(i:i)=' ' !将该字符置为空格
			else
				EXIT	!-有效字符不是SingleChar
			end if
			!------------------------------
		else
			EXIT
		end if
	end do
	!------------------------------------------------------------
	end subroutine KeepRealChar
	!=============================================================
	subroutine KeepNumChar(InStr)
	!------------------------------------------------------------
	!---保留InStr中的数字字符,其它字符全部变为空格
	!---吴徐平2013-07-20(wxp07@qq.com)
	!------------------------------------------------------------
	Implicit None
	Character(Len =*),Intent( INOUT ) :: InStr
	Character(Len =10):: NumChar='0123456789'
	!------------------------------------------------------------
	Integer ::i,j,flag
	!------------------------------------------------------------
	do i=1,LEN(InStr)
		flag=0
		!-------------------------------
		do j=1,LEN(NumChar)
			!-------------------------------
			if (InStr(i:i)==NumChar(j:j)) then
				flag=flag+1  !-识别为NumChar字符
				Exit
			end if
			!------------------------------
		end do
		!-------------------------------
		if (flag==0) then
			InStr(i:i)=' '  !-非NumChar字符,置为空格
		end if
		!------------------------------
	end do
	!------------------------------------------------------------
	end subroutine KeepNumChar
	!=============================================================
	subroutine TrimIndex(InStr,LeftIndex,RightIndex,Error)
	!------------------------------------------------------------
	!---识别InStr中左右有效可见字符(33-126)的索引
	!---如果Error==0,则识别正确
	!---吴徐平2013-07-20(wxp07@qq.com)
	!------------------------------------------------------------
	Implicit None
	Character(Len =*),Intent( IN ) :: InStr
	Integer,Intent( OUT)::LeftIndex,RightIndex,Error
	!------------------------------------------------------------
	Integer ::i
	LeftIndex=0
	RightIndex=LEN(InStr)+1
	!------------------------------------------------------------
	if (LEN(TRIM(InStr))>0) then
		do i=1,LEN(InStr),1
			if ((IACHAR(InStr(i:i)) >32 ).AND.(IACHAR(InStr(i:i)) <127) ) then
				LeftIndex=i !-左边有效可见字符(33-126)的索引
				EXIT
			end if
		end do
		!------------------------------------------------------------
		do i=LEN(InStr),1,-1
			if ((IACHAR(InStr(i:i)) >32 ).AND.(IACHAR(InStr(i:i)) <127 )) then
				RightIndex=i !-右边有效可见字符(33-126)的索引
				EXIT
			end if
		end do
		!--------------------------
		if ((LeftIndex>0 ).AND. (LeftIndex<=RightIndex) .AND. (RightIndex<=LEN(InStr)))then
			Error=0  !-操作正确
		else
			Error=-1 !-操作有误
		end if
		!--------------------------
	else
		Error=-1 !-字符串全部为空格或是空字符串
	end if
	end subroutine TrimIndex
	!=============================================================
	subroutine StringSplit(InStr,delimiter,StrArray,nsize)
	!----------------------------------------------
	!---将字符串InStr进行分割,结果放入StrArray中
	!---delimiter::分隔符号,例如';,,' 使用;和,分割字符串
	!---nsize:分割数目
	!---吴徐平2011-04-29(wxp07@qq.com)
	!----------------------------------------------
	implicit none
	character(len = *) , Intent( IN ) :: InStr
	character(len = *)  , Intent( IN ) :: delimiter
	character(len = LEN(InStr)),dimension(LEN(InStr)),Intent( OUT ) :: StrArray
	integer, Intent( OUT ) :: nsize ! Effective Size of StrArray
	integer:: i,j ! loop variable
	integer:: istart ! split index for Start Position
	nsize=0
	istart=1
	do i=1,LEN(InStr)
		do j=1,LEN(delimiter)
			if (InStr(i:i) == delimiter(j:j)) then
				if (istart == i) then
					istart=i+1 ! ---可防止分隔符相连的情况
				end if
				if (istart<i) then
					nsize=nsize+1
					StrArray(nsize)=InStr(istart:i-1)
					istart=i+1
				end if
			end if
		end do
	end do
	! ---匹配最后一个子字符串
	if (nsize>0) then
		if (istart<LEN(InStr)) then
			nsize=nsize+1
			StrArray(nsize)=InStr(istart:LEN(InStr))
		end if
	end if
	! ---如果无可分割的子字符串,则包含整个字符串为数组的第一元素
	if ( (nsize<1) .AND. (LEN(TRIM(InStr)) > 0 )) then
		nsize=1
		StrArray(1)=InStr
	end if
	end subroutine StringSplit
	!
	!=============================================================
	subroutine StrReplace(InStr,OldChar,NewChar,OutStr)
	!------------------------------------------------------------
	!---将字符串InStr中的字符串OldChar替换成NewChar
	!---结果放入字符串OutStr中
	!---吴徐平2013-07-20(wxp07@qq.com)
	!------------------------------------------------------------
	implicit none
	character(len = *) , Intent( IN ) :: InStr
	character(len = *) , Intent( IN ) :: OldChar
	character(len = LEN(OldChar)) , Intent( IN ) ::NewChar
	character(len = LEN(InStr)) , Intent( INOUT ) :: OutStr
	integer :: i  ! loop variable
	OutStr=InStr
	i=INDEX(OutStr,OldChar)
	do while(i>0)
		OutStr(i:i+LEN(OldChar)-1)=NewChar
		i=INDEX(OutStr,OldChar)
	end do
	end subroutine StrReplace
	!------------------------------------------------------------
	!=============================================================
	subroutine StrToRealArray4(InStr,RealArray,nsize)
	!------------------------------------------------------------
	Implicit None
	Character(Len = *), Intent( IN ) :: InStr
	Character(Len = LEN(InStr)):: Str_temp
	Integer:: i,j,Error,nsize
	Real::Num
	character(len =LEN(InStr)),dimension(LEN(InStr)):: StrArray
	Real(kind=4),dimension(LEN(InStr)),Intent(OUT) :: RealArray
	character(len = 4):: delimiter=' ;,	'
	Error=0
	nsize=0
	j=0
	Str_temp=InStr
	call KeepRealChar(Str_temp)
	!----------------------
	call  StringSplit(Str_temp,delimiter,StrArray,nsize)
	if (nsize>=1)then
		do i=1,nsize
			call KeepRealChar(StrArray(i))
			call StrToNum(StrArray(i),Num,Error)
			if (Error==0) then
				j=j+1
				RealArray(j)=Num
			end if
		end do
	end if

	nsize=j
	!------------------------------------------------------
	end subroutine StrToRealArray4
	!=============================================================
	subroutine StrToRealArray8(InStr,RealArray,nsize)
	!------------------------------------------------------------
	Implicit None
	Character(Len = *), Intent( IN ) :: InStr
	Character(Len = LEN(InStr)):: Str_temp
	Integer:: i,j,Error,nsize
	Real::Num
	character(len =LEN(InStr)),dimension(LEN(InStr)):: StrArray
	Real(kind=8),dimension(LEN(InStr)),Intent(OUT) :: RealArray
	character(len = 4):: delimiter=' ;,	'
	Error=0
	nsize=0
	j=0
	Str_temp=InStr
	call KeepRealChar(Str_temp)
	!----------------------
	call  StringSplit(Str_temp,delimiter,StrArray,nsize)
	if (nsize>=1)then
		do i=1,nsize
			call KeepRealChar(StrArray(i))
			write(*,*)	StrArray(i)
			call StrToNum(StrArray(i),Num,Error)
			if (Error==0) then
				j=j+1
				RealArray(j)=Num
			end if
		end do
	end if

	nsize=j
	!------------------------------------------------------
	end subroutine StrToRealArray8
	!=============================================================
	!=============================================================
	subroutine IsRealArrayString(InStr,Error)
	!------------------------------------------------------------
	!---测试字符串InStr转为RealArray类型的数组
	!---Error == 0 ::表示InStr可以转为RealArray数组,否则不能转换
	!---吴徐平2011-04-29(wxp07@qq.com)
	!------------------------------------------------------------
	Implicit None
	Character(Len = *), Intent( IN ) :: InStr
	Integer ,Intent( OUT ) :: Error
	Real,dimension(LEN(InStr)):: RealArray
	!------------------------------------------
	Integer::nsize
	Error=0
	nsize=0
	!------------------------------------------
	call StrToRealArray(InStr,RealArray,nsize)
	if (nsize>=1)then
		Error=0 !可以转为RealArray
	else
		Error=-1 !不可以转为RealArray
	end if
	!------------------------------------------------------
	end subroutine IsRealArrayString
	!
	!=============================================================
	subroutine GetFileRowColumn(FileName,HeadLine,Row,Column)
	!------------------------------------------------------------
	!---获取文本文件FileName的行数Row
	!---吴徐平2013-07-20(wxp07@qq.com)
	!------------------------------------------------------------
	Implicit None
	Character(Len = *), Intent( IN ) :: FileName
	Integer, Intent( out ) :: HeadLine  !---文件头的行数
	Integer , Intent( out ) :: Row !---文件行数Row
	Integer, Intent( out ) :: Column !---最大列数Column
	Character(Len = 1000) :: CLine
	Integer:: IOStatus=0
	Real,dimension(LEN(CLine)):: RealArray
	Integer:: nsize=0
	!---------------------------------------------
	Row=0
	HeadLine=0
	Column=0
	!---------------------------------------------
	!---获取Row和Column
	close(9001)
	!---先测试出文件行数和数据的最大列数
	open(unit=9001,file=FileName,status='OLD')
	Read( 9001 ,'(A1000)',iostat=IOStatus) CLine
	Do While (IOStatus == 0 )
		Row = Row + 1
		!---------------------------------------------
		call StrToRealArray(CLine,RealArray,nsize)
		!---------------------------------------------
		if (nsize>0 .AND.Column<nsize) then
			Column=nsize
		end if
		!---------------------------------------------
		Read( 9001 ,'(A1000)',iostat=IOStatus) CLine
		!---------------------------------------------
	End Do
	close(9001)
	!---------------------------------------------
	close(9001)
	!---------------------------------------------
	!---测试文件头HeadLine
	open(unit=9001,file=FileName,status='OLD')
	Read( 9001 ,'(A1000)',iostat=IOStatus) CLine
	!---------------------------------------------
	call StrToRealArray(CLine,RealArray,nsize)
	!---------------------------------------------
	Do While (IOStatus==0 .AND. nsize < Column )
		!---------------------------------------------
		HeadLine=HeadLine+1
		!---------------------------------------------
		Read( 9001 ,'(A1000)',iostat=IOStatus) CLine
		!---------------------------------------------
		call StrToRealArray(CLine,RealArray,nsize)
		!---------------------------------------------
	End Do
	close(9001)
	!---------------------------------------------
	end subroutine GetFileRowColumn
	!
	!=============================================================
	subroutine LoadFromFile4(FileName,Array2D,Row,Column,Error)
	!------------------------------------------------------------
	!---获取文本文件FileName的数据
	!---Array2D::大小(Row,Column),存放数据的二维可分配内存大小的数组
	!---Row::文件行数,Column::文件数据列数
	!---Error==0::表示读取文本文件中的数据正确,否则有误
	!---吴徐平2013-07-22(wxp07@qq.com)
	!------------------------------------------------------------
	Implicit None
	Character(Len = *), Intent( IN ) :: FileName
	Integer ,Intent( OUT ) ::Row,Column
	Real(kind=4),allocatable,Intent( OUT ) :: Array2D(:,:)
	Character(Len = 1000) :: CLine
	Real,dimension(LEN(CLine)):: RealArray
	Integer,Intent( OUT ):: Error
	Integer  :: TotalRow,HeadLine,nsize,i,j,IOStatus
	!---------------------------------------
	Row=0
	Column=0
	TotalRow=0
	HeadLine=0
	nsize = 0
	i = 0
	j=0
	Error = 0
	IOStatus = 0
	RealArray=0.0
	!---------------------------------------
	call GetFileRowColumn(FileName,HeadLine,TotalRow,Column)
	Row=TotalRow-HeadLine !只包含数据的行数
	!---------------------------------------
	if (Row>0 .AND. Column>0 ) then
		!--------------------------
		if (Allocated(Array2D)) then
			deallocate(Array2D)
		end if
		allocate(Array2D(Row,Column),stat=Error)
		Array2D=0.0 !初始化为0
		!------------------------
		if (Error==0) then
			!--------------------------
			close(9002)
			!---------------------------------------
			open(unit=9002,file=FileName,status='OLD')
			!---------------------------------------
			do i=1,TotalRow
				Read( 9002 , '(A1000)' ,iostat=IOStatus) CLine
				if (IOStatus==0 .AND. i>HeadLine) then
					!---------------------------------------
					call StrToRealArray(CLine,RealArray,nsize)
					!---------------------------------------
					if (nsize >0) then
						!---------------------------------------
						do j=1,nsize
							Array2D(i-HeadLine,j)=RealArray(j)
						end do
						!---------------------------------------
					end if
					!---------------------------------------
				end if
			end do
			!---------------------------------------
			close(9002)
			!---------------------------------------
		else
			Error=-1 !分配内存失败
		end if
		!---------------------------------------
	else
		Error=-1 !文本文件中没有数据
	end if
	!---------------------------------------
	end subroutine LoadFromFile4
	!
	!=============================================================
	!=============================================================
	subroutine LoadFromFile8(FileName,Array2D,Row,Column,Error)
	!------------------------------------------------------------
	!---获取文本文件FileName的数据
	!---Array2D::大小(Row,Column),存放数据的二维可分配内存大小的数组
	!---Row::文件行数,Column::文件数据列数
	!---Error==0::表示读取文本文件中的数据正确,否则有误
	!---吴徐平2013-07-22(wxp07@qq.com)
	!------------------------------------------------------------
	Implicit None
	Character(Len = *), Intent( IN ) :: FileName
	Integer ,Intent( OUT ) ::Row,Column
	Real(kind=8),allocatable,Intent( OUT ) :: Array2D(:,:)
	Character(Len = 1000) :: CLine
	Real,dimension(LEN(CLine)):: RealArray
	Integer,Intent( OUT ):: Error
	Integer  :: TotalRow,HeadLine,nsize,i,j,IOStatus
	!---------------------------------------
	Row=0
	Column=0
	TotalRow=0
	HeadLine=0
	nsize = 0
	i = 0
	j=0
	Error = 0
	IOStatus = 0
	RealArray=0.0
	!---------------------------------------
	call GetFileRowColumn(FileName,HeadLine,TotalRow,Column)
	Row=TotalRow-HeadLine !只包含数据的行数
	!---------------------------------------
	if (Row>0 .AND. Column>0 ) then
		!--------------------------
		if (Allocated(Array2D)) then
			deallocate(Array2D) !释放内存,重新分配
		end if
		allocate(Array2D(Row,Column),stat=Error)
		Array2D=0.0 !初始化为0
		!------------------------
		if (Error==0) then
			!--------------------------
			close(9002)
			!---------------------------------------
			open(unit=9002,file=FileName,status='OLD')
			!---------------------------------------
			do i=1,TotalRow
				Read( 9002 , '(A1000)' ,iostat=IOStatus) CLine
				if (IOStatus==0 .AND. i>HeadLine) then
					!---------------------------------------
					call StrToRealArray(CLine,RealArray,nsize)
					!---------------------------------------
					if (nsize >0) then
						!---------------------------------------
						do j=1,nsize
							Array2D(i-HeadLine,j)=RealArray(j)
						end do
						!---------------------------------------
					end if
					!---------------------------------------
				end if
			end do
			!---------------------------------------
			close(9002)
			!---------------------------------------
		else
			Error=-1 !分配内存失败
		end if
		!---------------------------------------
	else
		Error=-1 !文本文件中没有数据
	end if
	!---------------------------------------
	end subroutine LoadFromFile8
	!
	!=============================================================
	end module StrNum
	
	subroutine StringSplit(InStr,delimiter,StrArray,nsize)
	!----------------------------------------------
	!---将字符串InStr进行分割,结果放入StrArray中
	!---delimiter::分隔符号,例如';,,' 使用;和,分割字符串
	!---nsize:分割数目
	!---吴徐平2011-04-29(wxp07@qq.com)
	!----------------------------------------------
	implicit none
	character(len = *) , Intent( IN ) :: InStr
	character(len = *)  , Intent( IN ) :: delimiter
	character(len = LEN(InStr)),dimension(LEN(InStr)),Intent( OUT ) :: StrArray
	integer, Intent( OUT ) :: nsize ! Effective Size of StrArray
	integer:: i,j ! loop variable
	integer:: istart ! split index for Start Position
	nsize=0
	istart=1
	do i=1,LEN(InStr)
		do j=1,LEN(delimiter)
			if (InStr(i:i) == delimiter(j:j)) then
				if (istart == i) then
					istart=i+1 ! ---可防止分隔符相连的情况
				end if
				if (istart<i) then
					nsize=nsize+1
					StrArray(nsize)=InStr(istart:i-1)
					istart=i+1
				end if
			end if
		end do
	end do
	! ---匹配最后一个子字符串
	if (nsize>0) then
		if (istart<LEN(InStr)) then
			nsize=nsize+1
			StrArray(nsize)=InStr(istart:LEN(InStr))
		end if
	end if
	! ---如果无可分割的子字符串,则包含整个字符串为数组的第一元素
	if ( (nsize<1) .AND. (LEN(TRIM(InStr)) > 0 )) then
		nsize=1
		StrArray(1)=InStr
	end if
	end subroutine StringSplit

	subroutine StrReplace(InStr,OldChar,NewChar,OutStr)
	!------------------------------------------------------------
	!---将字符串InStr中的字符串OldChar替换成NewChar
	!---结果放入字符串OutStr中
	!---吴徐平2013-07-20(wxp07@qq.com)
	!------------------------------------------------------------
	implicit none
	character(len = *) , Intent( IN ) :: InStr
	character(len = *) , Intent( IN ) :: OldChar
	character(len = LEN(OldChar)) , Intent( IN ) ::NewChar
	character(len = LEN(InStr)) , Intent( INOUT ) :: OutStr
	integer :: i  ! loop variable
	OutStr=InStr
	i=INDEX(OutStr,OldChar)
	do while(i>0)
		OutStr(i:i+LEN(OldChar)-1)=NewChar
		i=INDEX(OutStr,OldChar)
	end do
	end subroutine StrReplace
