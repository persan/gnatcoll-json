import jsl

class Entry (jsl.Document):
	name = jsl.StringField(required = True)

class File (Entry):
	content=jsl.StringField(required = True)
	
class Directory (Entry):
	content= jsl.ArrayField(jsl.OneOfField([
		jsl.DocumentField(File,as_ref = True),
		jsl.DocumentField(jsl.RECURSIVE_REFERENCE_CONSTANT)]),
		required = True)



print Directory.get_schema(ordered = True)
