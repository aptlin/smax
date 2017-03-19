#!/bin/bash

# Check for git
command -v git >/dev/null 2>&1 || { echo >&2 "I require git but it's not installed.";
				    exit 1; }

# Check for emacs
command -v emacs >/dev/null 2>&1 || { echo >&2 "I require emacs but it's not installed.";
				      exit 1; }

# At least major version of 24
if [ ! `emacs --batch --eval "(prin1 emacs-major-version)"` -ge 24 ]; then
    echo "You need an emacs with major version of 24 or greater."
    exit 1;
fi

# if version 24, at least 24.4
if [ `emacs --batch --eval "(prin1 emacs-major-version)"` -eq 4 ]; then
    if [ ! `emacs --batch --eval "(prin1 emacs-minor-version)"` -ge 4]; then
	echo "You need an emacs with version 24.4 or greater."
	exit 1;
    fi
fi


# Now clone smax
if [ ! -d "smax" ]; then
    git clone https://github.com/sdll/smax.git
fi

echo "smax is installed. To use it, run this command in your terminal."
echo "emacs -q -l `pwd`/smax/init.el"
echo "or"
echo "run the smax.sh script created in this directory as ./smax.sh in the terminal."

echo "#!/bin/bash
emacs -q -l `pwd`/smax/init.el
#end" > smax.sh
chmod +x smax.sh
#end
