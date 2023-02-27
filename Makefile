isntall:
	pip install --upgrade pip &&\
    	pip install -r requirements.txt
        
format:
	black *.py
    
lint:
	pylint --disable=R,C Plumber_Example.ipynb
    
test:
	python -m pytest --vv --cov=hello Plumber_Example.ipynb