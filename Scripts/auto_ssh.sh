function rsync_upload_file() {
	tput setaf 2
	echo "Upload Time ..."
	tput setaf 0 
	echo "##############################"
	tput setaf 3
	echo "How many file you wanna upload ?"
	read time
	tput setaf 0
	echo "##################################"
	if (($time > 1));then
		tput setaf 4
		echo "Type folder name contains files to upload"
		read file
		if [ -d $file ]; then
			tput setaf 2
			echo "Folder Exists Continuing"
		elif [ ! -d $file ]; then
			tput setaf 1
			echo "Folder doesn't exits returning" && rsync_upload_file
		fi
		tput setaf 0
		tput setaf 3; echo "Where Do You Want To Upload ?"
		read where
		choice="repo";choice2="arch"
		if [ $where==$choice ];then
			echo "Uploading files $file"
			rsync -a -v $file -e ssh ybenel@frs.sourceforge.net:/home/frs/project/darkos-repo/DarkOs-Repo/x86_64/ --progress
		elif [ $where==$choice2 ]; then
			echo "Uploading files $file"
			rsync -a -v $file -e ssh ybenel@frs.sourceforge.net:/home/frs/project/darkos-arch/ --progress
		fi	
		
	elif (($time < 1));then
		tput setaf 5
		echo "Type File name To Upload"
		read name
		if [ -f $name ]; then
			tput setaf 2
			echo "File $name Exists Continuing"
		elif [ ! -f $name ]; then 
			tput setaf 1
			echo "File $name doesn't exists .. return" && rsync_upload_file
		fi
		tput setaf 0 
		echo "Uploading File $name"
		rsync -v -e ssh $name ybenel@frs.sourceforge.net:/home/frs/project/darkos-arch/

	fi
	
}


function ssh_frs() {
	username='ybenel'
	domain='shell.sourceforge.net'
	type='create'
	ssh -t $username@$domain $type


}


function which_one(){
	echo "What Do U Want To Do ? "
	read choice
	answer='code'
	answer2='edoc'
	if [ $choice == $answer ];then
		clear && ssh_frs
	elif [ $choice == $answer2 ];then
		clear && rsync_upload_file
	fi
}

which_one
