	!上面的程序可以很方便地读出文本文件中的数据,而且无须知道文件中数据的存放格式,
	!每行和每列的数据个数可以不一样,缺少的数据自动补为0.0,
	!自动识别文件头等信息,输出结果如下:
	!  http://www.xuebuyuan.com/1597280.html
	!------------------------------------------------------------
	!include"StrNum.F90"
	!--------------------------------------------------
	program main
	use StrNum
	implicit none
	integer :: count,i,j
	CHARACTER(len=100) :: Filename !命令行参数
	Integer::Error,HeadLine,Row,Column
	real(kind=8), allocatable :: Array2D(:,:)
	!!-----------------------------------------------------------------
	!count = command_argument_count() !获取主程序命令行的输入参数的个数
	!!------------------------------------------------------------------
	!if (count>0) then
	!do i=1,count
	!	CALL get_command_argument(i, Filename)
	write(*,*)'------------------------------------------'
	filename="D:\datafile\ec79-15\interdata\output\DP\2000Bergeron700.txt"
	call GetFileRowColumn(Filename,HeadLine,Row,Column) !获取文件行,列
	write(*,*)"HeadLine=",HeadLine,"Row=",Row,"Column=",Column
	write(*,*)'------------------------------------------'
	call LoadFromFile(trim(Filename),Array2D,Row,Column,Error)
	if (Error==0)then
		write(*,*)"Array2D(Row,Column):"
		do j=1,Row
			write(*,*)Array2D( j,1:Column)
		end do
		write(*,*)'------------------------------------------'
	else
		write(*,*)"文件读写有误"
	end if
	!	end do
	!else
	!	write(*,*) 'You should input an argument!'
	!end if
	!---------------------------------------------
	if (Allocated(Array2D)) then
		deallocate(Array2D)
	end if
	!---------------------------------------------
	end program


